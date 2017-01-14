(ns structured-data)



(defn do-a-thing
  [x]
  (let [x (+ x x)]
   (Math/pow x x)))


(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
  (+ a c))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [[[x1 y1] [x2 y2] :as rectangle]]
  (- y2 y1))


(defn width [[[x1 y1] [x2 y2] :as rectangle]]
  (- x2 x1))


(defn square? [[[x1 y1] [x2 y2] :as rectangle]]
  (= (- x2 x1) (- y2 y1)))


(defn area [[[x1 y1] [x2 y2] :as rectangle]]
  (* (- x2 x1) (- y2 y1)))


(defn contains-point? [[[x1 y1] [x2 y2] :as rectangle] [px py]]
  (and (<= x1 px x2)
       (<= y1 py y2)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))


(defn title-length [book]
  (count (:title book)))

(defn title-length2 [{t :title}]
  (count t))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))


(defn alive? [author]
  (not (contains? author  :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [coll] (get coll 1))]
    (map second collection)))

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
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book  :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (cond
                (contains? author :death-year) (str " (" (:birth-year author)
                                                    " - " (:death-year author) ")")
                (contains? author :birth-year) (str " (" (:birth-year author) " - )")
                :else "")]
    (str name  years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string
  [books]
  (if (empty? books)
    "No books."
    (let [count (count books)]
      (apply str (concat [count (if (<= count 1) " book. " " books. ")]
                         (concat (interpose ". "(map book->string books)) ["."]))))))



(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name
  [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (set (filter alive? authors)))

(defn has-a-living-author? [book]
   (not (empty? (filter alive? (:authors book)))))


(defn books-by-living-authors
  [books]
  (filter has-a-living-author? books))

; %________%






