(ns structured-data)

(defn do-a-thing [x]
  (let [plussa (+ x x)]
    (Math/pow plussa plussa)))

(defn spiff [v] (+ (get v 0) (get v 2)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v] (let [[a b c] v] (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] (
        let [eka (get rectangle 0) toka (get rectangle 1)]
          (max (- (first eka) (first toka)) (- (- (first eka) (first toka))) )
                          ))

(defn height [rectangle] (
  let [eka (get rectangle 0) toka (get rectangle 1)]
          (max (- (second eka) (second toka)) (- (- (second eka) (second toka))) )
                          ))

(defn square? [rectangle] (if (= (height rectangle) (width rectangle)) true false))

(defn area [rectangle] (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [eka (get rectangle 0) toka (get rectangle 1)]
    (if (and (<= (first eka) (first point) (first toka)) (<= (second eka) (second point) (second toka)) ) true false
      )  ))

(defn contains-rectangle? [outer inner]
  (if (and (contains-point? outer (first inner)) (contains-point? outer (second inner))  ) true false))

(defn title-length [book] (count (:title book)))

(defn author-count [book] (count (:authors book)))

(defn multiple-authors? [book] (if (> (author-count book) 1) true false))

(defn add-author [book new-author] (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author] (if (= (:death-year author) nil) true false ))

(defn element-lengths [collection] (map count collection))

(defn second-elements [collection] (map second collection))

(defn titles [books] (map :title books))


(defn increasing [x y]
  (if (or (<= x y) (= y nil)) true false))

(defn decreasing [x y]
  (if (or (>= x y) (= y nil)) true false))

(defn monotonic? [a-seq] (if
            (or
              (apply = (map increasing a-seq (rest a-seq))) (apply = (map decreasing a-seq (rest a-seq) )) ) true false ))


(defn stars [n] (apply str (repeat n "*")))

(defn toggle [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq] (if (= (count a-seq) (count (into #{} a-seq) )) false true) )

(defn old-book->new-book [book] (assoc book :authors (into #{} (:authors book)) ))

(defn has-author? [book author] (if (contains? (:authors book) author) true false ))

(defn authors [books] (into #{} (apply concat (map :authors books))))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author] (if (nil? (:birth-year author)) (str(:name author)) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")" ) ))


(defn author-names [authors]
  (map author->string authors))

(defn add-pilkku [x y]
  (if (= y nil) x (str x ", ") ))

(defn add-piste [x]
  (str x ". ") )

(defn authors->string [authors] (let [nimet (author-names authors)]

 (str  (apply str (map add-pilkku nimet (rest nimet) ) ) (last nimet) )) )

(defn book->string [book] (str (:title book) ", written by " (authors->string (:authors book))))

(defn kirjojen-kuvaukset [books]
  (map book->string books))

(defn remove-from-end [s end]
  (if (.endsWith s end)
      (.substring s 0 (- (count s)
                         (count end)))
    s))


(defn books->string [books] ( let [kirjat (kirjojen-kuvaukset books)]

 (remove-from-end
 (cond
   (= (count books) 0) "No books."
   (= (count books) 1) (str "1 book. " (apply str (map add-piste kirjat)) )
    :else  (str (count books) " books. " (apply str (map add-piste kirjat)) ) )  " ")) )

(defn books-by-author [author books]

  (filter (fn [book] (if (contains? (:authors book) author) true false ) ) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (if (= (:name author) name) true false ) ) authors )))

(defn living-authors [authors]
  (filter (fn [author] (if (= (:death-year author) nil) true false )) authors ))


(defn has-a-living-author? [book]

  (if (empty? (living-authors (:authors book)) ) false true)
  )

(defn books-by-living-authors [books]
  (filter (fn [book] (if (has-a-living-author? book) true false) ) books)
  )

; %________%
