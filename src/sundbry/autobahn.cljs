(ns ^{:doc "Autobahn JS interface"}
  sundbry.autobahn)

(def ^:private ab js/autobahn)

(defn- autobahn-debug 
  "Enable/disable Autobahn debug logging"
  [enable?]
  (set! js/AUTOBAHN_DEBUG enable?))

(defn create 
  "Create a new proxy to a node"
  [{:keys [on-open on-close debug?] :as conf}]
  (let [instance
        (merge 
          {:debug? true
           :ws-uri nil
           :realm "default"
           :connection nil
           :session (atom nil)
           :on-open nil
           :on-close nil}
          conf)
        conn (ab.Connection. (clj->js {:url (:ws-uri instance)
                                       :realm (:realm instance)}))
        instance (assoc instance :connection conn)
        wrap-on-open (fn [session]
                       (reset! (:session instance) session)
                       (when (some? on-open)
                         (on-open session)))
        wrap-on-close (fn [reason message]
                        (reset! (:session instance) nil)  
                        (when (some? on-close)
                          (on-close reason message)))]
    (set! (.-onopen conn) wrap-on-open)
    (set! (.-onclose conn) wrap-on-close)
    (autobahn-debug (boolean debug?))
    instance))

(defn connect
  [proxy]
  (.log js/console (str "Connecting to " (:ws-uri proxy)))
  (.open (:connection proxy))
  proxy)

(defn disconnnect
  [proxy]
	(.log js/console (str "Disconnecting from " (:ws-uri proxy)))
	(.close (:connection proxy))
	proxy)

(defn- default-error-handler 
  [error]
	(.log js/console (str "[remote error] " (.-message error))))

(defn- parse-json-error 
  "Returns a javascript Error instance from an Autobahn json error"
  [json-error]
	(new js/Error
		(if (nil? (.-detail json-error))
				(.-desc json-error)
				(str (.-desc json-error) ": " (.-detail json-error)))))

(defn call
  "Execute an RPC call
  @param proxy {:session}
  @param cmd (list function args...)
  @param cb-success (json-result)
  @param cb-error (json-error)"
  ([proxy rpc-uri args] (call proxy rpc-uri args (constantly nil) default-error-handler))
  ([proxy rpc-uri args cb-success] (call proxy rpc-uri args cb-success default-error-handler))
  ([proxy rpc-uri args cb-success cb-error]
   (let [sess @(:session proxy)]
     (if (nil? sess)
       (throw (new js/Error "Not connected"))
       ; exec RPC
       (let [args (into-array args)
             call (.call sess rpc-uri args)]
         (.then call
                #(cb-success (js->clj %))
                #(cb-error (parse-json-error %))))))))
