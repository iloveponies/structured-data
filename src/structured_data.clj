(ns structured-data)

(defn do-a-thing [x]
  (let [n (+ x x)]
    (Math/pow n n)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
  (+ a c))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[x y] point
        [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (>= (author-count book) 2))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [helper (fn [x] (get x 1))]
    (map helper collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [countset (count (set a-seq))
        countseq (count a-seq)]
    (not= countseq countset)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [namea (:name author)]
  (cond
    (contains? author :death-year) (str namea " (" (:birth-year author) " - " (:death-year author) ")")
    (contains? author :birth-year) (str namea " (" (:birth-year author) " - )")
    :else namea)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [bookname (:title book)
        authors (authors->string (:authors book))]
    (str bookname ", written by " authors)))
                                 

(defn books->string [books]
  (let [first-part (let [counter (count books)]
                    (cond 
                      (= 0 counter) "No books"
                      (= 1 counter) "1 book"
                      :else (str counter " books")))
        second-part (if (= first-part "No books")
                      ""
                      (str ". " (apply str (interpose ". " (map book->string books)))))]
    (str first-part second-part ".")))
      

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [has-alive? (-> book :authors living-authors)]
    (not (empty? has-alive?))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
