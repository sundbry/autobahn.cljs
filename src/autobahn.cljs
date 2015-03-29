(ns ^{:doc "Autobahn JS interface"}
  autobahn)

(declare change-state)

(defn- autobahn-debug 
  "Enable/disable Autobahn debug logging"
  [wamp-on ws-on]
	(.debug js/ab "wamp" wamp-on)
	(.debug js/ab "ws" ws-on))

(defn create 
  "Create a new proxy to a node"
  [conf]
  (let [instance
        (merge 
          {:debug-wamp true
           :debug-ws true
           :rpc-base-uri nil
           :ws-uri nil
           :state :disconnected ; [:disconnected :connecting :connected :disconnecting]
           :session nil}
          conf)]
    (autobahn-debug (boolean (:debug-wamp instance)) (boolean (:debug-ws instance)))
    instance))

(defn- st-connecting
  [proxy args]
	(.log js/console (str "Connecting to " (:ws-uri proxy)))
	(.connect js/ab (:ws-uri proxy)
		; on connect
		(fn [session]
			(change-state proxy :connected [session]))
		; on close
		(fn [code reason]
			(change-state proxy :disconnected [code reason])))
	proxy)

(defn- st-connected 
  [proxy [session]]
	(.log js/console "Connected.")
	(assoc proxy :session session))

(defn- st-disconnecting 
  [proxy args]
	(.log js/console (str "Disconnecting from " (:ws-uri proxy)))
	(.close (:session proxy))
	proxy)

(defn- st-disconnected 
  [proxy [code reason]]
	(.log js/console (str "Disconnected[" code "]: " reason))
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

(defn command
  "Execute a command on the butterbuffet server
   @param proxy {:session}
   @param cmd (list function args...)
   @param cb-success (json-result)
   @param cb-error (json-error)"
	([proxy cmd] (command proxy cmd #() default-error-handler))
	([proxy cmd cb-success] (command proxy cmd cb-success default-error-handler))
	([proxy cmd cb-success cb-error]
		(let [sess (:session proxy)]
			(if (nil? sess)
				(throw (new js/Error "Not connected"))
				(.then
					; exec RPC
					(let [cmd (if (symbol? cmd) (str cmd) cmd)
							  cmd (if (string? cmd) (reverse (into '() (.split cmd " "))) cmd)
								args (into-array (cons (rpc-uri proxy (first cmd))
                                       (rest cmd)))]
								;(.call sess (util/rpc-uri proxy cmd))
						(.apply (.-call sess) sess args))
					#(cb-success (js->clj %))
					#(cb-error (parse-json-error %)))))))

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
