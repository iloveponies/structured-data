(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (* x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

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
  (let [[[x1 y1] [x2 y2]] rectangle] 
    (Math/abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[p1 p2] point
        [[x1 y1][x2 y2]] rectangle]
    (if (and (>= x2 p1 x1) (>= y2 p2 y1)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
    (if (and (contains-point? outer (point x1 y1))
             (contains-point? outer (point x1 y2))
             (contains-point? outer (point x2 y1))
             (contains-point? outer (point x2 y2)))
      true false)))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (= (author-count book) 1) false true))

(defn add-author [book new-author]
  (let [original (:auhtors book)
        new (conj original new-author)]
  (assoc book :auhtors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [f (fn [val] (get val 1))]
    (map f collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem) 
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (apply concat (authors books))))

(defn author->string [author]
  (let [years (fn [birth, death]
                (if death 
                  ;; True
                  (str " (" birth " - " death ")") 
                  ;; False
                  (if birth
                    ;; True
                    (str " (" birth " - )")
                    ;; False
                    "")))]
  (str (:name author) (years (:birth-year author) (:death-year author)))))

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
