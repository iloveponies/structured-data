(ns structured-data)

(defn do-a-thing [x]
  (let  [dx (+ x x)]
    (Math/pow dx dx)))


(defn spiff [v]
  (+ (get v 0) (get v 2)))


(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))


(defn point [x y]
  [x y])


(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (int (Math/sqrt (Math/pow (- x2 x1) 2)))))


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (int (Math/sqrt (Math/pow (- y2 y1) 2)))))


(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
  (let  [[px py] point
         [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 px x2)
         (<= y1 py y2)
    )))


(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))
    ))


(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (> (author-count book) 1))


(defn add-author [book new-author]
  (let [auts     (:authors book)
        newauts  (conj auts new-author)]
    (assoc book :authors newauts)
    ))


(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [tooth (fn [x] (get x 1))]
    (map tooth collection)
    ))


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
  (> (count a-seq)
    (count (set a-seq))))


(defn old-book->new-book [book]
  (let [a-vec (:authors book)]
    (assoc book :authors (set a-vec))))


(defn has-author? [book author]
  (contains? (:authors book) author))


(defn authors [books]
   (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [namestr (:name author)
        datestr (if (contains? author :birth-year)
                    (str " (" (:birth-year author) " - " (:death-year author) ")")
                    (str ""))]
    (str namestr datestr)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (let [countStr (cond
                     (> (count books) 1) (str (count books) " books.")
                     (> (count books) 0) (str (count books) " book.")
                     :else "No books.")
        booksStr (apply str (interpose ". " (map book->string books)))]
    (str countStr (if (empty? booksStr)
                      ""
                      (str " " booksStr ".")))
    ))


(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))


(defn author-by-name [name allauthors]
  (first (filter (fn [x] (= name (:name x))) allauthors)))


(defn living-authors [allauthors]
  (filter (fn [x] (alive? x)) allauthors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))


; %________%
