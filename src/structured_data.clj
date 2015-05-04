(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
    (Math/pow x x)))


(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[a d t] v]
   (+ a t)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- x1 x2) (- y1 y2))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (*  (- x1 x2) (- y1 y2))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp) (>= x2 xp) (<= y1 yp) (>= y2 yp))))


(defn contains-rectangle? [outer inner]
  (let [[pi1 pi2] inner]
    (and (contains-point? outer pi1) (contains-point? outer pi2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-book-authors (conj (:authors book) new-author)]
    (assoc book :authors new-book-authors)))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [helper (fn [x] (second x))]
    (map helper collection)))

(defn titles [books]
  (let [fun (fn [x] (:title x))]
    (map fun books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
 (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors
  [books]
  (let [authors (fn [book] (:authors book))]
    (set (apply concat (map authors books))))
  )

(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
   (let [author_name (:name author)
         author_years (when (not (nil? (:birth-year author))) (str " (" (:birth-year author) " - " (:death-year author) ")"))]
     (str author_name author_years)))


(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))


(defn book->string [book]
  (apply str (interpose ", written by " (vector (:title book) (authors->string (:authors book))))))

(defn books->string [books]
  (apply str (apply str
         (if (= 0 (count books))
           (str "No books")
           (if (< 1 (count books))
             (str (count books) " books. ")
             (str (count books) " book. ")))
         (interpose ". " (map book->string books))
          ) "."))


(defn books-by-author [author books]
  (let [auth_filter (fn [book] (has-author? book author))]
  (filter auth_filter books)))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
