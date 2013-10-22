(ns structured-data
  (:use clojure.math.numeric-tower))

(defn do-a-thing [x]
  (let [x+x (+ x x)]
    (Math/pow x+x x+x)))

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

(defmacro with-point [[x y] pt & body]
  `(let [[~x ~y] ~pt]
     ~@body))

(defmacro with-rect-old [[x0 y0 x1 y1] rect & body]
  `(let [rr# ~rect
         ~x0 (get (get rr# 0) 0)
         ~y0 (get (get rr# 0) 1)
         ~x1 (get (get rr# 1) 0)
         ~y1 (get (get rr# 1) 1)]
     ~@body))

(defmacro with-rect [[x0 y0 x1 y1] rect & body]
  `(let [[[~x0 ~y0] [~x1 ~y1]] ~rect]
     ~@body))

(defn width [rectangle]
  (with-rect (x0 y0 x1 y1) rectangle
    (abs (- x1 x0))))

(defn height [rectangle]
  (with-rect (x0 y0 x1 y1) rectangle
    (abs (- y1 y0))))

(defn square? [rectangle]
  (= (width  rectangle)
     (height rectangle)))

(defn area [rectangle]
  (* (width  rectangle)
     (height rectangle)))

(defn contains-point? [rectangle point]
  (with-point (x y) point
    (with-rect (x0 y0 x1 y1) rectangle
      (and (<= x0 x x1)
           (<= y0 y y1)))))

(defn contains-rectangle? [outer inner]
  (with-rect (ax0 ay0 ax1 ay1) inner
    (with-rect (bx0 by0 bx1 by1) outer
      (and (<= bx0 ax0 ax1 bx1)
           (<= by0 ay0 ay1 by1)))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (get author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

; not used
(defn every-pair? [p xs]
  (or (empty? xs)
      (let [[x & xs] xs]
        (or (empty? xs)
            (and (p x (first xs))
                 (every-pair? p (rest xs)))))))

(defn pairwise [f xs]
  ((fn [acc xs]
     (if (empty? xs)
         acc
         (let [[x & tail] xs]
           (if (empty? tail)
               acc
               (recur (conj acc (f x (first tail)))
                      (rest tail))))))
   [] xs))

(defn monotonic? [a-seq]
  (let [sgn #(cond (< % 0) -1 (> % 0) 1 :else 0)]
    (not (= 2 (count (remove #{0} (set (map sgn (pairwise - a-seq)))))))))

(defn stars [n]
  (reduce str "" (repeat n "*")))

(defn toggle [a-set elem]
  ((if (contains? a-set elem) disj conj) a-set elem))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (reduce concat #{} (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author)
       (let [birth (:birth-year author)
             death (:death-year author)]
         (when (or birth death)
           (str " (" birth " - " death ")")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn pluralize [n singular plural]
  (if (= n 0)
      (str "No " plural)
      (str n " " (if (= n 1) singular plural))))

(defn books->string [books]
  (apply str
   (interpose " "
    (map #(str % ".")
     (list*
      (pluralize (count books) "book" "books")
      (map book->string books))))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter #(not (:death-year %)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
