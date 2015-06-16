(ns erinite.renderer.freactive
  (:require-macros
    [freactive.macros :refer [rx debug-rx]])
  (:require
    [erinite.template.compile :refer [apply-xforms]]  
    [freactive.core :as fr]))


(defn no-op
  "Transformation that does nothing.
   Applies to templates: all
   Child transforms:     applied
   Read params:          none
   Narrow scope:         no
   Expected args:        none"
  [template parameters scoped-parameters action-arguments child-transformations]
  (apply-xforms
    template
    child-transformations
    parameters
    scoped-parameters))


; parameters is an atom
(defn content
  "Transformation that replaces content with content from params keyed by `action-arguments`.
   Applies to templates: [node-key attrs-map & content]
   Child transforms:     not applied
   Read params:          scoped
   Narrow scope:         no
   Expected args:        [korks]"
  [[elem attrs & content :as template] parameters scoped-parameters action-arguments child-transformations]
  (let [[korks] action-arguments
        fetch   (if (vector? korks) get-in get)
        fratom  (fr/atom (fetch @scoped-parameters korks))
        watch   (add-watch
                  scoped-parameters
                  ::content
                  (fn [_ _ old-val new-val]
                    (let [value (fetch new-val korks)]
                      (when-not (identical?  (fetch old-val korks) value)
                        (reset! fratom value)))))]
    [elem attrs (rx @fratom)]))


(defn content-global
  "Transformation that replaces content with content from params keyed by `action-arguments`.
   Applies to templates: [node-key attrs-map & content]
   Child transforms:     not applied
   Read params:          root
   Narrow scope:         no
   Expected args:        [korks]"
  [[elem attrs & content :as template] parameters scoped-parameters action-arguments child-transformations]
  (let [[korks] action-arguments
        fetch   (if (vector? korks) get-in get)
        fratom  (fr/atom (fetch @parameters korks))
        watch   (add-watch
                  parameters
                  ::content
                  (fn [_ _ old-val new-val]
                    (let [value (fetch new-val korks)]
                      (when-not (identical?  (fetch old-val korks) value)
                        (reset! fratom value)))))]
    [elem attrs (rx @fratom)]))


(defn clone-for
  "Transformation that duplicates content for each item in a sequence keyed by `action-arguments`. 
   Applies to templates: [node-key attrs-map & content]
   Child transforms:     applied
   Read params:          scoped
   Narrow scope:         yes
   Expected args:        [korks]"
  [[elem attrs & content :as template] parameters scoped-parameters action-arguments child-transformations]
  (let [[korks] action-arguments
        fetch   (if (vector? korks) get-in get)
        fratom  (fr/atom (fetch @scoped-parameters korks))
        watch   (add-watch
                  scoped-parameters
                  ::content
                  (fn [_ _ old-val new-val]
                    (let [value (fetch new-val korks)]
                      (when-not (identical?  (fetch old-val korks) value)
                        (reset! fratom value)))))]
    (rx
      (apply
        vector
        elem
        attrs
        (reduce
          (fn [children item]
            (into
              children
              (apply-xforms
                (into [] content) ;Transform relative to child
                child-transformations
                parameters
                (atom item)))) ; Scoped by item
          []
          @fratom)))))


(defn set-attr
  "Transformation that sets an attribute to value taken from params keyed by `action-arguments`.
   Applies to templates: [node-key attrs-map & content]
   Child transforms:     applied
   Read params:          scoped
   Narrow scope:         no
   Expected args:        [attr-name korks]"
  [[elem attrs & content :as template] parameters scoped-parameters action-arguments child-transformations]
  (let [[attr-name korks] action-arguments
        fetch   (if (vector? korks) get-in get)
        fratom  (fr/atom (fetch @scoped-parameters korks))
        watch   (add-watch
                  scoped-parameters
                  ::content
                  (fn [_ _ old-val new-val]
                    (let [value (fetch new-val korks)]
                      (when-not (identical?  (fetch old-val korks) value)
                        (reset! fratom value)))))]
    (apply
      vector
      elem
      (rx (assoc attrs attr-name @fratom))
      (apply-xforms
        content
        child-transformations
        parameters
        scoped-parameters))))

(defn- from-class-map
  [value attrs]
  (->> value  
       (filter val)                 ; Remove any classes that are not set to true
       (map (comp name first))      ; Get the class names as strings
       (into                        ; Append the new class names to the end of the existing classes
         (if-let [classes (:class attrs)]
           [classes]
           []))                     
       (clojure.string/join " ")    ; Join them together into a single space separated string
       (assoc attrs :class)))


(defn set-class-map
  "Transformation that adds class attributes based on params keyed by `action-arguments`.
   Applies to templates: [node-key attrs-map & content]
   Child transforms:     applied
   Read params:          scoped
   Narrow scope:         no
   Expected args:        [korks]"
  [[elem attrs & content :as template] parameters scoped-parameters action-arguments child-transformations]
  (let [[korks] action-arguments
        fetch   (if (vector? korks) get-in get)
        fratom  (fr/atom (from-class-map (fetch @scoped-parameters korks) attrs))
        watch   (add-watch
                  scoped-parameters
                  ::content
                  (fn [_ _ old-val new-val]
                    (let [value (fetch new-val korks)]
                      (when-not (identical?  (fetch old-val korks) value)
                        (reset! fratom (from-class-map value attrs))))))]
    (apply
      vector
      elem
      (rx @fratom)
      (apply-xforms
        content
        child-transformations
        parameters
        scoped-parameters))))


(defn- from-class-string
  [class-name attrs classes should-set?]
  (if-let [class-name (and class-name (name class-name))]
    (assoc
      attrs
      :class
      (if should-set? 
        ;; Add class if not already present
        (if-not (> (.indexOf (str classes) class-name) -1)
          (str classes (when classes " ") class-name)
          classes)
        ;; Remove class if not already absent
        (.replace (str classes) (js/RegExp. class-name "g") "")))
    attrs))


(defn set-class
  "Transformation that sets a class based on params keyed by `action-arguments`.
   Applies to templates: [node-key attrs-map & content]
   Child transforms:     applied
   Read params:          scoped
   Narrow scope:         no
   Expected args:        [class-name should-set-korks]"
  [[elem {classes :class :as attrs} & content :as template] parameters scoped-parameters action-arguments child-transformations]
  (let [[class-name should-set-korks] action-arguments
        fetch   (if (vector? should-set-korks) get-in get)
        fratom  (fr/atom (from-class-string class-name attrs classes (fetch @scoped-parameters should-set-korks)))
        watch   (add-watch
                  scoped-parameters
                  ::content
                  (fn [_ _ old-val new-val]
                    (let [value (fetch new-val should-set-korks)]
                      (when-not (identical?  (fetch old-val should-set-korks) value)
                        (reset! fratom (from-class-string class-name attrs classes value))))))]
    (apply
      vector
      elem
      (rx @fratom)
      (apply-xforms
        content
        child-transformations
        parameters
        scoped-parameters))))


(defn append-content
  "Transformation that appends content to the element, based on params keyed by `action-arguments`.
   Applies to templates: [node-key attrs-map & content]
   Child transforms:     applied (only to existing content, not to appended content)
   Read params:          scoped
   Narrow scope:         no
   Expected args:        [korks]"
  [[elem attrs & content :as template] parameters scoped-parameters action-arguments child-transformations]
  (let [[korks] action-arguments
        fetch   (if (vector? korks) get-in get)
        fratom  (fr/atom (fetch @scoped-parameters korks))
        watch   (add-watch
                  scoped-parameters
                  ::content
                  (fn [_ _ old-val new-val]
                    (let [value (fetch new-val korks)]
                      (when-not (identical?  (fetch old-val korks) value)
                        (reset! fratom value)))))]
    (apply
      vector
      elem
      attrs
      (conj
        (into []
          (apply-xforms
            content
            child-transformations
            parameters
            scoped-parameters))
        (rx @fratom)))))


(defn prepend-content
  "Transformation that prepends content to the element, based on params keyed by `action-arguments`.
   Applies to templates: [node-key attrs-map & content]
   Child transforms:     applied (only to existing content, not to prepended content)
   Read params:          scoped
   Narrow scope:         no
   Expected args:        [korks]"
  [[elem attrs & content :as template] parameters scoped-parameters action-arguments child-transformations]
  (let [[korks] action-arguments
        fetch   (if (vector? korks) get-in get)
        fratom  (fr/atom (fetch @scoped-parameters korks))
        watch   (add-watch
                  scoped-parameters
                  ::content
                  (fn [_ _ old-val new-val]
                    (let [value (fetch new-val korks)]
                      (when-not (identical?  (fetch old-val korks) value)
                        (reset! fratom value)))))]
    (apply
      vector
      elem
      attrs
      (rx @fratom)    
      (apply-xforms
        content
        child-transformations
        parameters
        scoped-parameters))))


(defn set-element-type
  "Transformation that sets the element type based on params keyed by `action-arguments`.
   Applies to templates: [node-key attrs-map & content]
   Child transforms:     applied
   Read params:          scoped
   Narrow scope:         no
   Expected args:        [korks]"
  [[elem attrs & content :as template] parameters scoped-parameters action-arguments child-transformations]
  (let [[korks] action-arguments
        fetch   (if (vector? korks) get-in get)
        fratom  (fr/atom (fetch @scoped-parameters korks))
        watch   (add-watch
                  scoped-parameters
                  ::content
                  (fn [_ _ old-val new-val]
                    (let [value (fetch new-val korks)]
                      (when-not (identical?  (fetch old-val korks) value)
                        (reset! fratom value)))))]
    (apply
      vector
      (rx @fratom)    
      attrs
      (apply-xforms
        content
        child-transformations
        parameters
        scoped-parameters))))


(def transforms
  {:nop               no-op
   :content           content
   :content-global    content-global
   :clone-for         clone-for
   :set-attr          set-attr  
   :set-classes       set-class-map
   :set-class         set-class
   :append-content    append-content
   :prepend-content   prepend-content
   :set-element-type  set-element-type})
