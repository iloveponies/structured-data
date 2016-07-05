(ns structured-data)

(defn do-a-thing [x]
  (Math/pow (+ x x) (+ x x)))

(defn spiff [v]
  (if (and (= (get v 0) nil) (= (get v 2) nil))
    "?"
    (+ (get v 2) (get v 0))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (and (= (get v 0) nil) (= (get v 2) nil))
    "?"
    (let [[a b c] v]
    (+ a c))))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> x1 x2)
      (- x1 x2)
      (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y1 y2)
      (- y1 y2)
      (- y2 y1))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y1 y2)
      (if (> x1 x2)
      (= (- x1 x2) (- y1 y2))
      (= (- x2 x1) ((- y2 y1))))
      (if (> x1 x2)
      (= (- x1 x2) (- y1 y2))
      (= (- x2 x1) (- y2 y1))))))


(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y1 y2)
      (if (> x1 x2)
      (* (- x1 x2) (- y1 y2))
      (* (- x2 x1) ((- y2 y1))))
      (if (> x1 x2)
      (* (- x1 x2) (- y1 y2))
      (* (- x2 x1) (- y2 y1))))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x3 y3] point]
      (and (<= x1 x3) (>= x2 x3) (<= y1 y3) (>= y2 y3)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[x3 y3] [x4 y4]] inner]
      (and (contains-point? (rectangle [x1 y1] [x2 y2])
                 (point x3 y3)) (contains-point? (rectangle [x1 y1] [x2 y2])
                 (point x4 y4))))))


(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
(count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (count (get book :authors))))

(defn add-author [book new-author]
  (let [original book
      new      (assoc original :authors new-author)]
  (assoc original :authors (conj (original :authors) new-author))))



(defn alive? [author]
 (not (contains? author :death-year)))

(defn element-lengths [collection]
   (map count collection))

(defn second-elements [collection]
  (let [modde (fn [x] (get x 1))]
  (map modde collection)))

(defn titles [books]
  (let [asda (fn [x] (x :title))]
    (map asda books)))

(defn monotonic? [a-seq]
  (if (apply <= a-seq)
    (if (apply < a-seq)
      true
      true)
    (if (apply >= a-seq)
      (if (apply > a-seq)
        true
        true)
      false)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= a-seq (seq (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
 (set (map :name (apply clojure.set/union (map :authors books)))))

(defn author->string [author]
  (let [asd (fn [x] (cond
              (contains? author :death-year) [(author :name) " (" (author :birth-year) " - " (author :death-year) ")"]
              (contains? author :birth-year) [(author :name) " (" (author :birth-year) " - )"]
              :else [(author :name)]))]
  (apply str (asd author))))

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str [(book :title) ", written by " (authors->string (:authors book))]))

(defn books->string [books]
  (cond
  (= 0 (count books)) "No books."
  (= 1 (count books)) (apply str [(count books) " book. " (apply str (interpose ". " (map book->string books))) "."])
  :else (apply str [(count books) " books. " (apply str (interpose ". " (map book->string books))) "."])))

(defn books-by-author [author books]
 (filter (fn [x] (has-author? x author)) books))


(defn author-by-name [name authors]
   (let [asd (fn [x] (first (filter (fn [x] (= (x :name) name)) x)))]
     (asd authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (> (count (filter (fn [x] (alive? x)) (book :authors))) 0))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
