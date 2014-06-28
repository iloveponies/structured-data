(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)))

(defn spiff [v]
  (let [first (first v)
        third (nth v 2)]
    (+ first third)))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[f s t] v]
    (+ f t)))


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
  ( =  (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle)
     (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2)
         (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (first inner))
       (contains-point? outer (last inner))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [n-book (assoc book :authors
                      (conj (get book :authors) new-author))]
    n-book))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

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
  (not (= (count (set a-seq))
          (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))) )


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [b-date (:birth-year author)
        d-date (:death-year author)
        dates (or b-date d-date)]
    (str (:name author)
         (if (or b-date d-date)
           (str " (" b-date " - " d-date ")")))))

(defn authors->string [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [cnt (cond
             (empty? books) "No books"
             ( = 1 (count books)) "1 book"
             :else (str (count books) " books"))
        bstring (map book->string books)
        bks (clojure.string/join ". " bstring )]
    (if ( = (count bstring) 0)
      (str cnt ".")
      (str cnt ". " bks "."))))

(defn books-by-author [author books]
  (filter (fn [x] (contains? (:authors x) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (nil? (:death-year x))) authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
