(ns ^{:doc "Autobahn JS interface"}
  sundbry.autobahn
  (:require
   [cljs.core.async :as async])
  (:require-macros 
    [cljs.core.async.macros :refer [go go-loop]]))

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
     (when (nil? sess)
       (throw (new js/Error "Not connected")))
     ; exec RPC
     (let [call (if (map? args)
                  (.call sess rpc-uri (js/Array. 0) (clj->js args))
                  (.call sess rpc-uri (into-array args)))]
       (.then call
              #(cb-success (js->clj %))
              #(cb-error (parse-json-error %)))))))

(defn call-async
  "Execute an RPC call using core.async. 
   Puts the response or an instance of Error on the returned channel."
  [proxy rpc-uri args]
  ; TODO buffer response(s), incremental results
  (let [chan (async/chan)]
    (call proxy rpc-uri args 
          (fn [response]
            (async/put! chan response)
            (async/close! chan))
          (fn [error]
            (async/put! chan error)
            (async/close! chan)))
    chan))

(defn subscribe
  "Subscribe to an event URI"
  ([proxy event-uri handler-fn] (subscribe proxy event-uri handler-fn (constantly nil) default-error-handler))
  ([proxy event-uri handler-fn cb-success] (subscribe proxy event-uri handler-fn cb-success default-error-handler))
  ([proxy event-uri handler-fn cb-success cb-error]
   (let [sess @(:session proxy)]
     (when (nil? sess)
       (throw (new js/Error "Not connected")))
     (let [sub (.subscribe sess event-uri (fn [args kw-args meta-data]
                                            (handler-fn (js->clj args)
                                                        (js->clj kw-args)
                                                        (js->clj meta-data))))]
       (.then sub
              #(cb-success %)
              #(cb-error (parse-json-error %)))))))

(defn unsubscribe
  "Unsubscribe from an event URI"
  [proxy subscription cb-success cb-error]
  (let [sess @(:session proxy)]
    (when (nil? sess)
      (throw (new js/Error "Not connected")))
    (let [unsub (.unsubscribe sess subscription)]
      (.then unsub
             #(cb-success %)
             #(cb-error (parse-json-error %))))))

(defn subscribe-async
  "Subscribe to an event URI using core.async.
   Puts events onto the provided channel.
   Returns a channel to receive the subscription object, or an Error."
  [proxy event-uri event-chan]
  (let [sub-chan (async/chan 1)]
    (subscribe proxy event-uri
               (fn [args kw-args meta-data]
                 (let [data (or kw-args args)]
                   (async/put! event-chan data)))
               (fn [subscription]
                 (async/put! sub-chan subscription)
                 (async/close! sub-chan))
               (fn [error]
                 (async/put! sub-chan error)
                 (async/close! sub-chan)))))

(defn unsubscribe-async
  "Unsubscribes from an event URI.
   Returns a channel that closes on unsubscription, or recieves an Error."
  [proxy subscription]
  (let [unsub-chan (async/chan 1)]
    (unsubscribe proxy subscription
                 (fn [success]
                   (async/close! unsub-chan))
                 (fn [error]
                   (async/put! unsub-chan error)
                   (async/close! unsub-chan)))))
