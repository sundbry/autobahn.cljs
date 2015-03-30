(ns ^{:doc "Autobahn JS interface"}
  sundbry.autobahn)

(def ^:private ab js/autobahn)

(declare change-state)

(defn- autobahn-debug 
  "Enable/disable Autobahn debug logging"
  [enable?]
  (set! js/AUTOBAHN_DEBUG enable?))

(defn create 
  "Create a new proxy to a node"
  [conf]
  (let [instance
        (merge 
          {:debug? true
           :rpc-base-uri nil
           :ws-uri nil
           :realm "default"
           :state :disconnected ; [:disconnected :connecting :connected :disconnecting]
           :connection nil
           :session nil}
          conf)
        conn (ab.Connection. (clj->js {:url (:ws-uri instance)
                                       :realm (:realm instance)}))
        instance (assoc instance :connection conn)]
    (set! (.-onopen conn)
          (fn [session]
            (change-state instance :connected [session])))
    (set! (.-onclose conn)
          (fn [reason details]
            (change-state instance :disconnected [reason details])))
    (autobahn-debug (boolean (:debug? instance)))
    instance))

(defn- st-connecting
  [proxy args]
  (.log js/console (str "Connecting to " (:ws-uri proxy)))
  (.open (:connection proxy))
  proxy)

(defn- st-connected 
  [proxy [session]]
	(.log js/console "Connected.")
	(assoc proxy :session session))

(defn- st-disconnecting 
  [proxy args]
	(.log js/console (str "Disconnecting from " (:ws-uri proxy)))
	(.close (:connection proxy))
	proxy)

(defn- st-disconnected 
  [proxy [reason details]]
	(.log js/console (str "Disconnected:" reason ". " details))
	(assoc proxy :session nil))

(defn- change-state
  "The transition function"
	([proxy new-state] (change-state proxy new-state []))
	([proxy new-state args]
		(let [state (:state proxy)
          transition #(assoc proxy :state new-state)
          hook #((get (:state-actions proxy) new-state) %)]
			(when (= :disconnected state)
				(when (= :connecting new-state)
					(hook (st-connecting (transition) args))))
			(when (= :connecting state)
				(when (= :connected new-state)
					(hook (st-connected (transition) args)))
				(when (= :disconnected new-state)
					(hook (st-disconnected (transition) args))))
			(when (= :connected state)
				(when (= :disconnecting new-state)
					(hook (st-disconnecting (transition) args))))
			(when (= :disconnecting state)
				(when (= :disconnected new-state)
					(hook (st-disconnected (transition) args)))))))

;; (proxy (map :state (proxy))) -> proxy
(defn do-connect 
  "Connect to host with callbacks"
  [proxy state-actions]
	(-> proxy (assoc :state-actions state-actions) (change-state :connecting)))

;; (proxy) -> proxy
(defn do-disconnect [proxy]
  "Disconnect from host"
	(change-state proxy :disconnecting))

(defn default-error-handler 
  [error]
	(.log js/console (str "[remote error] " (.-message error))))

(defn- parse-json-error 
  "Returns a javascript Error instance from an Autobahn json error"
  [json-error]
	(new js/Error
		(if (nil? (.-detail json-error))
				(.-desc json-error)
				(str (.-desc json-error) ": " (.-detail json-error)))))


(defn- rpc-uri 
  "Returns URI to WAMP RPC resource"
  [proxy rpc-route]
	(str (:rpc-base-uri proxy) rpc-route))

(defn call
  "Execute an RPC call
  @param proxy {:session}
  @param cmd (list function args...)
  @param cb-success (json-result)
  @param cb-error (json-error)"
  ([proxy rpc-route args] (call proxy rpc-route args (constantly nil) default-error-handler))
  ([proxy rpc-route args cb-success] (call proxy rpc-route args cb-success default-error-handler))
  ([proxy rpc-route args cb-success cb-error]
   (let [sess (:session proxy)]
     (if (nil? sess)
       (throw (new js/Error "Not connected"))
       ; exec RPC
       (let [uri (rpc-uri proxy rpc-route)
             args (into-array args)
             call (.call sess uri args)]
         (.then call
                #(cb-success (js->clj %))
                #(cb-error (parse-json-error %))))))))

(def ^:private global-proxy nil)
(def ^:private connected-continuations (list))

(defn- wait-connected-k
  "Run a function once connected."
  [F]
	(set! connected-continuations (conj connected-continuations F)))

(defn connect 
  "Create and connect to the global proxy"
  [conf]
	(set! global-proxy (create conf))
	(do-connect global-proxy {
		:connecting
			(fn [remote]
				(set! global-proxy remote))
		:connected
			(fn [remote]
				(set! global-proxy remote)
				; Run waiting continuations
				(loop [conts connected-continuations]
					(when (not (empty? conts))
							((first conts) global-proxy)
							(recur (rest conts))))
				(set! connected-continuations (list)))
		:disconnecting
			(fn [remote]
				(set! global-proxy remote))
		:disconnected
			(fn [remote]
				; Events are unsubscribed automatically on close
				(set! global-proxy remote)
				(set! connected-continuations (list)))}))

(defn disconnect []
	(do-disconnect global-proxy))

;; Apply F with a proxy
; ((proxy)) -> (F)
(defn with-proxy [F]
	(when (= nil global-proxy)
		(throw (js/Error "Proxy is nil")))
	(let [state (:state global-proxy)]
		(if (= :connected state)
			(F global-proxy)
			(if (= :connecting state)
				(wait-connected-k #(F %))
				(throw (new js/Error "Proxy is disconnected"))))))
