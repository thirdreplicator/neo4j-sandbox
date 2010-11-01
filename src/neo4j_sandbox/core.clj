(ns neo4j-sandbox.core)

; DIAGRAM OF NEO4J NODE SPACE
;
; (top-node)
;     |
; (:customers)
;   |        \
; (:bob)     (:jane)
;

(use ['neo4j :exclude ['start 'shutdown]])
(use 'inflections)


; Utilities
(defn pluralize-keyword [kw]
  (-> kw name pluralize keyword))

(defn singularize-keyword [kw]
  (-> kw name singularize keyword))
 
; Pretty printing of nodes.
(defmethod print-method org.neo4j.kernel.impl.core.NodeProxy [node writer] 
  (.write writer 
    (str "#<" (.toString node) " " (properties node) ">")))

; Pretty printing of relationships.
(defmethod print-method org.neo4j.kernel.impl.core.RelationshipProxy [relationship writer] 
  (.write writer 
    (str "#<" (.toString relationship) " " 
         (str (.name (.getType relationship))) ">")))



; ***** START UP AND SHUT DOWN *****

(defn reload []
  (load "/neo4j_sandbox/core"))

(defn boot-neo4j []
  (neo4j/start "/home/david/clj/neo4j-sandbox/neo4j"))

(defn shutdown-neo4j []
  (neo4j/shutdown))

; ***** ***** ***** ***** ***** 
; ***** BASIC OPERATIONS *****
; ***** ***** ***** ***** ***** 

; CREATE TABLE
(defn create-root-type [type]
  (with-tx
    (let [root-node (new-node {:category type})]
      (relate (top-node) type root-node))))

(defn get-root-node [node-type]
  (-> (top-node)
  (.getSingleRelationship (relationship node-type) outgoing)
  (.getEndNode)))

; CREATE NODE
(defn add-typed-node [node-type-singular attr]
  "E.g.: (add-typed-node :customer {:id 1 :name \"Ralph\" :job \"Farmer\"})"
  (with-tx
    (let [node-type-plural (pluralize-keyword node-type-singular)
         base-node-for-category (get-root-node node-type-plural)
         new-typed-node (new-node)]
     (relate base-node-for-category node-type-singular new-typed-node)
     (properties new-typed-node attr))))

; INDEX listing
(defn select-all [node-type]
  "Get all nodes of a certain (plural) type.
E.g. (select-all :customers)"
  (let [root-node (get-root-node node-type)
        node-type-singular (singularize-keyword node-type)]
    (seq (traverse root-node breadth-first (depth-of 1) all-but-start {node-type-singular outgoing}))))


; SELECT with "WHERE" clause on node attributes by traversing.
(defn select-where 
  "By traversing the graph, select nodes of a (plural) type where values are specified in a hash-map
   E.g. Find a node by the name of \"Bob\":
   (select-where :customers {:name \"Bob\"})"
  [node-type attr]  
  (let [node-type-singular (singularize-keyword node-type)
        root-node          (get-root-node node-type)]
    (seq (find-by-props root-node node-type-singular attr))))

; SELECT with * using Lucene.

; UPDATE
(defn update-node 
  "Update a mapped-node with an attribute map."
  ([mapped-node]
    (update-mapped-node mapped-node))
  ([node attr]
    (if (map? node)
      (update-node (merge node attr))
      (update-node (map-node node) attr))))

; DELETE
(defn delete-node [node]
  (with-tx (node-delete node)))

; DELETE FROM ... WHERE
(defn delete-where 
  "Delete all nodes of a certain type that satisfy the equality conditions specified in the attribute map."
  [root-node-plural attr]
  (doseq [node (select-where root-node-plural attr)]
    (delete-node node)))

(defn select-one [node-type-singular attr]
  (let [node-type-plural (pluralize-keyword node-type-singular)]
     (-> (select-where node-type-plural attr) first)))

; Classify
(defn classify

"Classify a node by keyword.
  Example:
    (classify cypher :bad-guys)"

  [node category]
  (let [root     (get-root-node category)
        singular (singularize-keyword category)]
    (with-tx 
      (relate root singular node))))

(defn rel

"Relate two nodes together.
  Example:
    (rel trinity :loves neo)"

  [n1 rel-kw n2]
  (with-tx
    (relate n1 rel-kw n2)))
  

(defn walk [& opts]
  (let [{:keys [start-node method depth ok-rels]
         :or
         {start-node          (top-node)
          method              breadth-first
          depth               end-of-graph
          ok-rels             []
         }} opts
         rel-map (reduce #(assoc %1 %2 outgoing) {} ok-rels)
         depth          (if (= end-of-graph depth) 
                            end-of-graph
                            (depth-of depth))]
    (seq (traverse start-node
                   method
                   depth
                   all-but-start 
                   rel-map))))

; Select friend of a friend of "Bob"

; Select all subgroups of admin.

; Select all subgroups of admin, who are also a subgroup of business
;  development.


; Select all subgroups of admin, who are friends of friend of "Bob",
;  who do not like bunnies, aged less than 30 years old.
;  



; ***** Relationships *****

(defn rels [node]
  (seq (relationships node)))

(defn incoming-relationships [node]
  (seq (relationships node incoming)))

(defn outgoing-relationships [node]
  (seq (relationships node outgoing)))

(defn all-relationships [node]
  (seq (relationships node both)))

; ***** APP-SPECIFIC ****

; GET ALL CUSTOMERS
(defn all-customers []
  (select-all :customers))

(defn add-customer [attr]
  (add-typed-node :customer attr))

(defn stringify-customer [c]
  (str "[" (property c "name") ", " 
           (property c "age" ) ", "
           (property c "id" ) "]"))

(defn print-customers []
 (with-tx
  (let [customer-root (get-root-node :customers)]
    (doall (map #(stringify-customer %) 
                (traverse customer-root
                          breadth-first
                          (depth-of 1)
                          all-but-start
                          {:customer outgoing}))))))

(defn knows [n1 n2]
   (rel n1 :knows n2))

