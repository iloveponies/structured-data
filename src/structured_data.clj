(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [fst (first v)
        trd (get v 2)]
    (+ fst trd)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ b]]
  (+ a b))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[a b]]
  (Math/abs (- (first a) (first b))))

(defn height [[a b]]
  (Math/abs (- (second a) (second b))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[a b] [x y]]
  (and (<= x (Math/abs (- (first a) (first b))))
       (<= y (Math/abs (- (second a) (second b))))))

(defn contains-point? [[a b] [x y]]
  (let [[max-x min-x] (sort > [(first a) (first b)])
        [max-y min-y] (sort > [(second a) (second b)])]
    (and (>= max-x x min-x) (>= max-y y min-y))))

(defn contains-rectangle? [outer [a b]]
  (and (contains-point? outer a)
       (contains-point? outer b)))

(defn title-length [{title :title}]
  (count title))

(defn author-count [{authors :authors}]
  (count authors))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new]
  (update-in book [:authors] #(conj % new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a]
  (or (apply >= a) (apply <= a)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a element]
  (if (contains? a element)
    (disj a element)
    (conj a element)))

(defn contains-duplicates? [a]
  (boolean (some #(> % 1) (vals (frequencies a)))))

(defn old-book->new-book [book]
  (update-in book [:authors] set))

(defn has-author? [book author]
  (contains? (get-in book [:authors]) author))

(defn authors [books]
  (reduce conj #{} (flatten (map seq (map :authors books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string
  [{:keys [name birth-year death-year] :as author}]
  (cond
   (contains? author :death-year)
     (format "%s (%s - %s)" name birth-year death-year)
   (contains? author :birth-year)
     (format "%s (%s - )" name birth-year)
   :else name))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [{:keys [title authors]}]
  (format "%s, written by %s" title (authors->string authors)))

(defn books->string [books]
  (let [book-count (count books)
        count-str (cond
                   (> 1 book-count) "No books"
                   (= 1 book-count) "1 book"
                   (< 1 book-count) (str book-count " books"))]
    (apply str (concat (interpose ". " (cons count-str (map book->string books))) '(".")))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (or (first (filter #(= (:name %) name) authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (boolean (some alive? (:authors book))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
