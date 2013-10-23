(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[x y] point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if(and (<= x1 x x2) (<= y1 y y2)) true false))))

(defn contains-rectangle? [outer inner]
  (let [[lower upper] inner]
    (if(and (contains-point? outer lower) (contains-point? outer upper)) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (== 1 (count (:authors book))) false true))

(defn add-author [book new-author]
  (let [newA (:authors book)]
    (assoc book :authors (conj newA new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [x] (get x 1))]
    (map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply >= a-seq) (apply <= a-seq)) true false))

(defn stars [n]
  (let [staaars (repeat n "*")]
    (apply str staaars)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if(== (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [az (set (map :authors books))]
    (apply clojure.set/union az)))

(defn all-author-names [books]
  (let [aa (authors books)]
    (set (map :name aa))))

(defn author->string [author]
  (let [nam (:name author)]
    (let [years (if (contains? author :birth-year) (if (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")") (str " (" (:birth-year author) " - )")) "")]
      (str nam years))))

(defn authors->string [authors]
  (let [names (map author->string authors)]
  (apply str (interpose ", " names))))

(defn book->string [book]
  (let [bookname (:title book)]
    (let [auths (authors->string (:authors book))]
    (str bookname ", written by " auths))))

(defn books->string [books]
  (let [starter (if (= (count books) 0) "No books." (if (= (count books) 1) "1 book. " (str (count books) " books. ")))]
    (let [bweks (apply str (interpose ". " (map book->string books)))]
    (str starter bweks (if (= (count books) 0) "" ".")))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first(filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [the-living (living-authors (:authors book))]
    (if (empty? the-living) false true)))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
