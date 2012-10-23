(ns structured-data)

(defn do-a-thing [x]
  (let [xx (* 2 x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (- (first (second rectangle))
     (first (first rectangle))))

(defn height [rectangle]
  (- (second (second rectangle))
     (second (first rectangle))))

(defn square? [rectangle]
  (= (width rectangle) 
     (height rectangle)))

(defn area [rectangle]
  (* (height rectangle) 
     (width rectangle)))

(defn contains-point? [rectangle point]
  (and (<= (first (first rectangle)) (first point) (first (second rectangle))) 
       (<= (second (first rectangle)) (second point) (second (second rectangle)))))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (first inner))
       (contains-point? outer (second inner))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authorlist (conj (:authors book) new-author)]
    (assoc book :authors authorlist)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [x] (first (rest x))) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or 
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (cond 
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) 
          (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (set (map :authors books))))

(defn all-author-names [books]
  (set (concat (map :name (seq (authors books))))))
  

(defn author->string [author]
  (cond 
    (nil? (:birth-year author)) (apply str [(:name author)])
    :else (apply str [(:name author) " (" (:birth-year author) " - " (:death-year author) ")"])))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str [(:title book) ", written by " (authors->string (:authors book))]))

(defn books->string [books]
  (let [bookcount (count books)
        bookstrings (apply str (interpose ". " (map book->string books)))]
    (cond
      (= bookcount 0) "No books."
      (= bookcount 1) (apply str ["1 book. " (book->string (get books 0)) "."])
      (> bookcount 1) (apply str [bookcount " books. " bookstrings "."]))))

(defn books-by-author [author books]
  ;________; i miss haskell and flip compositions
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
