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

(defn spiff-destructuring [[fst _ trd]]
  (+ fst trd))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[bl tr]]
  (Math/abs (- (first bl) (first tr))))

(defn height [[bl tr]]
  (Math/abs (- (second bl) (second tr))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[bl tr] [x y]]
  (and (<= x (Math/abs (- (first bl) (first tr))))
       (<= y (Math/abs (- (second bl) (second tr))))))

(defn contains-point? [[bl tr] [x y]]
  (let [[max-x min-x] (sort > [(first bl) (first tr)])
        [max-y min-y] (sort > [(second bl) (second tr)])]
    (and (>= max-x x min-x) (>= max-y y min-y))))

(defn contains-rectangle? [outer [ibr itr]]
  (and (contains-point? outer ibr)
       (contains-point? outer itr)))

(defn title-length [{title :title}]
  (count title))

(defn author-count [{authors :authors}]
  (count authors))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (update-in book [:authors] #(conj % new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (boolean (some #(> % 1) (vals (frequencies a-seq)))))

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

