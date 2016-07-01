(ns structured-data)

(defn do-a-thing [x]
  (let [two_x (+ x x)]
    (Math/pow two_x two_x)))

(defn spiff [v]
  (let [first_element (get v 0)
        third_element (get v 2)]
    (+ first_element third_element)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x1 x2 x3] v]
    (+ x1 x3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let[[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [sq_width (width rectangle)
        sq_height (height rectangle)]
    (== sq_width sq_height)))

(defn area [rectangle]
  (let [sq_width (width rectangle)
        sq_height (height rectangle)]
    (* sq_width sq_height)))

(defn between? [v]
  (let [[a x1 x2] v]
    (and (<= x1 a) (<= a x2))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [a b] point]
(and (between? [a x1 x2]) (between? [b y1 y2]))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [point1 point2] inner]
    (and (contains-point? outer point1) (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
   (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec_ele (fn [v] (get v 1))]
      (map sec_ele collection)))

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
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-names (fn [book] (:authors book))]
    (apply clojure.set/union (map author-names books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author_name (:name author)
        birth_year (:birth-year author)
        death_year (:death-year author)]
    (if (contains? author :death-year)
        (str author_name " (" birth_year " - " death_year ")")
        (if (contains? author :birth-year)
          (str author_name " (" birth_year " - )")
          (str author_name)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book_title (:title book)
        author_names (authors->string (:authors book))]
        (str book_title ", written by " author_names)))

(defn books->string [books]
  (let [book_count (count books)]
    (if (> book_count 1)
      (str (apply str book_count " books. " (interpose ". " (map book->string books))) ".")
      (if (== book_count 1)
        (str (apply str book_count " book. " (map book->string books)) ".")
      "No books."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))


(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
