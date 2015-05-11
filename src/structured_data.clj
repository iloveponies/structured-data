(ns structured-data)

(defn do-a-thing [x]
  (let [squared (+ x x)]
    (Math/pow squared squared)))

(defn spiff [v]
  (let [first (get v 0 0) third (get v 2 0)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[f _ t] v]
    (+ 
     (if (nil? f) 0 f)
     (if (nil? t) 0 t))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[bx by] [tx ty]] rectangle]
    (- tx bx)))

(defn height [rectangle]
  (let [[[bx by] [tx ty]] rectangle]
    (- ty by)))

(defn square? [rectangle]
  (let [w (width rectangle) h (height rectangle)]
    (if (= w h) true false)))

(defn area [rectangle]
  (let [w (width rectangle) h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[[bx by] [tx ty]] rectangle [px py] point]
    (if (and 
         (<= bx px tx)
         (<= by py ty))
      true false)))

(defn contains-rectangle? [outer inner]
  (let [[b t] inner]
    (if (and
         (contains-point? outer b)
         (contains-point? outer t))
      true 
      false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
 (let [authors (get book :authors)]
   (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count (seq collection)))

(defn second-elements [collection]
  (let [get-second-element (fn [col] (get col 1))]
    (map get-second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq)
      (apply <= a-seq)))

(defn stars [n]
  (reduce str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq)) (count a-seq))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (let [author-name (:name author)
        book-authors (:authors book)]
    (if (= nil (first (filter 
                 #(= (get % :name) author-name) 
                 book-authors)))
      false
      true)))

(defn authors [books]
  (reduce clojure.set/union (map #(get % :authors) books)))

(defn all-author-names [books]
  (set (map #(get % :name) (authors books))))

(defn author->string [author]
  (let [name (get author :name)
        birth-year (get author :birth-year)
        death-year (get author :death-year "")]
    (if (nil? birth-year)
      (str name)
      (str name " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " 
                    (map author->string authors ))))

(defn book->string [book]
  (let [book-title (get book :title)
        authors-string (authors->string (get book :authors))]
    (str book-title ", written by " authors-string)))

(defn books->string [books]
  (cond
   (empty? books) "No books."
   (== 1 (count books)) (str "1 book. " 
                             (book->string (first books)) ".")
   :else 
   (str (count books) " books. " 
        (apply str (interpose ", " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= (get % :name) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  ((complement empty?) (living-authors (get book :authors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
