(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first _ third] v]
    (+ first third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[bottom-left top-right] rectangle
        [x1 y1] bottom-left
        [x2 y2] top-right]
    (- x2 x1)))

(defn height [rectangle]
  (let [[bottom-left top-right] rectangle
        [x1 y1] bottom-left
        [x2 y2] top-right]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle)
     (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[bottom-left top-right] rectangle
        [x y] point
        [x1 y1] bottom-left
        [x2 y2] top-right]
    (and
     (<= x1 x x2)
     (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [coll] (get coll 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq)
          (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (if (contains? author :birth-year)
                (str " (" (:birth-year author) " - " (:death-year author) ")")
                "")]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (empty? books) "No books."
   :else (str (count books) " book"
             (if (> (count books) 1)
               "s"
               "")
             ". "
             (apply str (interpose " "
                                   (map (fn [book] 
                                          (str
                                           (book->string book)
                                           "."))
                                        books))))))

(defn books-by-author [author books]
  (filter (fn [book]
            (contains? (:authors book) author))
          books))

(defn author-by-name [name authors]
  (first
   (filter (fn [author]
             (= (:name author) name))
           authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not
   (empty?
    (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
