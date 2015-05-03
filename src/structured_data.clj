(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point]
     (and (<= x1 p1 x2)  (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [orig-authors (get book :authors)]
  (assoc book :authors (conj orig-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [c] (get c 1))]
    (map get-second collection)))

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
  (not (= (count (set a-seq)) (count a-seq))))


(defn old-book->new-book [book]
  (let [authors (set (get book :authors))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [{name :name birth :birth-year death :death-year } author]
    (if (= birth nil )
      (str name)
      (str name " (" birth " - " death ")"))))

(defn authors->string [authors]
  (apply str ( interpose ", " (map author->string authors))))


(defn book->string [book]
  (let [{title :title authors :authors} book]
    (str title ", written by " (authors->string authors))))


(defn books->string [books]
  (let [formatted-string (fn [book-count book-str] (str (apply str book-count " " book-str ". "  (interpose ". " (map book->string books))) "."))]
    (cond
     (= (count books) 0) (str "No books.")
     (= (count books) 1) (formatted-string 1 "book")
     (>= (count books) 2) (formatted-string (count books) "books"))))

(defn books-by-author [author books]
  (filter #(has-author? %1 author) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %1) name) authors)))

(defn living-authors [authors]
  (set (filter alive? authors)))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
