(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
   (+ first third)))

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
  (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [point-x point-y] point
        contains-x (<= x1 point-x x2)
        contains-y (<= y1 point-y y2)]
  (and contains-x contains-y)))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
  (and (contains-point? outer point1)
       (contains-point? outer point2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (get book :authors)
        new-authors (conj old-authors new-author)]
  (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
  (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  ))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [old-authors (get book :authors)
        new-authors (set old-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (let [authors (get book :authors)]
  (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name  (get author :name)
        birth (get author :birth-year)]
  (if (nil? birth)
    name
    (let [death        (get author :death-year)
         year-span    (str "(" birth " - " death ")")]
    (str name " " year-span)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (let [title         (get book :title)
        authors       (authors->string (get book :authors))]
   (str title ", written by " authors)))

(defn book-descriptions [books]
  (apply str (interpose ", " (map book->string books))))

(defn books->string [books]
  (cond
    (= (count books) 0) "No books."
    (= (count books) 1) (str "1 book. " (book-descriptions books) ".")
    (> (count books) 1) (str (count books) " books. " (book-descriptions books) ".")
   ))

(defn books-by-author [author books]
  (let [has-author?  (fn [book] (contains? (get book :authors) author))]
   (filter has-author? books)))

(defn author-by-name [name authors]
  (let [has-name? (fn [author] (= name (get author :name)))]
  (first (filter has-name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
