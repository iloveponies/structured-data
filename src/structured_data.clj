(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (let [ a (first v)
        b (nth v 2)]
    (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [ [a _ c] v ]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
 (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [px1 py1]  point]
    (if (and (<= x1 px1 x2) (<= y1 py1 y2))
      true
      false
      )))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[xx1 yy1] [xx2 yy2]] inner]
    (if (and (contains-point? outer [xx1 yy1]) (contains-point? outer [xx2 yy2]))
      true
      false
      )))

(defn title-length [book]
 (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (count (:authors book)))
    true
    false
    ))

(defn add-author [book new-author]
 (let [authors (conj (:authors book) new-author)]
  (assoc book :authors authors)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true
    ))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= (concat a-seq)) (apply >= (concat a-seq))))

(defn stars [n]
  (apply str (repeat n "*") ) )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)
        set-count (count a-set)
        seq-count (count a-seq)]
    (if (= set-count seq-count)
      false
      true 
      )))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (let [auths (:authors book)]
    (if (contains? auths author)
      true
      false)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [ auth (:name author) ]
    (if (contains? author :birth-year)
      (str auth " (" (:birth-year author) " - " (:death-year author) ")")
      auth
      )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [ct (count books)
        bks (if (= 1 ct)
              "book"
              "books")]
    (if (= ct 0)
      "No books."
      (str ct " " bks ". " (apply str (interpose ". "(map book->string books))) "."))))

(defn books-by-author [author books]
  (let [pred  (fn [x] (if (contains? (:authors x) author)
                        true
                        false))]
    (filter pred books)))

; (defn books-by-author [author books]
;   (let [pred (partial has-author? nil author)]
;     (filter pred books)))

(defn author-by-name [name authors]
   (let [pred (fn [x] (if (= name (:name x))
                             true
                             false))] 
     (first (filter pred authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [auths (living-authors (:authors book))]
    (if (or (nil? auths) (empty? auths))
      false
      true )))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
