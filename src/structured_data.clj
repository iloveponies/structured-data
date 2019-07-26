(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (let [first-val (get v 0)
        third-val (get v 2)]
    (cond (nil? first-val)
          ,,nil
          (nil? third-val)
          ,,first-val
          :t
          ,,(+ first-val third-val))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[first-val _ third-val]]
  (cond (nil? first-val)
        ,,nil
        (nil? third-val)
        ,,first-val
        :t
        ,,(+ first-val third-val)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn abs [x]
  (if (< x 0)
    (- x)
    x))

(defn width [rectangle]
  (let [[[x_1 _] [x_2 _]] rectangle]
    (abs (- x_1 x_2))))

(defn height [rectangle]
  (let [[[_ y_1] [_ y_2]] rectangle]
    (abs (- y_1 y_2))))

(defn square? [rectangle]
  (= (width rectangle)
     (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2)
         (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-point-1 inner-point-2] inner]
    (and (contains-point? outer inner-point-1)
         (contains-point? outer inner-point-2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [v] (get v 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq)
     (count (set a-seq))))

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
        years (str "(" (:birth-year author) " - " (:death-year author) ")")]
    (str name (if (:birth-year author) (str " " years)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [cnt (count books)
        counter (cond (= cnt 0) "No books."
                      (= cnt 1) "1 book."
                      :t (str cnt " books."))]
    (apply str counter
           (map #(str " " (book->string %) ".") books))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
