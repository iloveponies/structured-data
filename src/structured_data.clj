(ns structured-data)

(defn do-a-thing [x]
  (let [thin (+ x x)](Math/pow thin thin)))

(defn spiff [v]
  (let [x (get v 0) y (get v 2)](+ x y)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b ] v](+ a b)))

(defn point [x y]
  [x y])

; returns a nested vector [[x1 y1][x2 y2]]
(defn rectangle [bottom-left top-right]
  (let [x bottom-left y top-right](vector x y)))

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle](- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle](- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle](= (- y2 y1)(- x2 x1))))

(defn area [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle](* (- y2 y1)(- x2 x1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle [p1 p2] point]
    (and (<= x1 p1 x2)(<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
        (and (<= x1 x3 x4 x2)(<= y1 y3 y4 y2))))

(defn title-length [book]
  (let [title (get book :title)](count title)))

(defn author-count [book]
  (let [authors (get book :authors)](count authors)))

(defn multiple-authors? [book]
  (let [authors (get book :authors)](> (count authors) 1)))

(defn add-author [book new-author]
  (update-in book [:authors] conj new-author))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(let [[_ second] %] second) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)(apply >= a-seq)))

(defn stars [n]
  (apply str (map #(let [_ % star "*"] star) (range n))))

(defn toggle [a-set elem]
  (cond
     (not (contains? a-set elem))(conj a-set elem)
     (contains? a-set elem)(disj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

; user=> book1
; {:title "a horse", :authors ["jimmy" "jimmy" "barry"], :publisher {:name "hall", :address ["london" "toronto"]}}
; get nested elements:
; (get-in book1 [:publisher :address])
; ["london" "toronto"]
; set nested items
; (assoc-in book1 [:authors] "hello")
; {:title "a horse", :authors "hello", :publisher {:name "hall", :address ["london" "toronto"]}}
(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc-in book [:authors] (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (into #{} (apply concat (map :authors books))))

(defn all-author-names [books]
  (let [all-authors
          (fn [book] (map :name (:authors book)))]
    (set (apply concat (map all-authors books)))))

(defn author->string [author]
  (let [name (author :name)
        birth (if (not= nil (author :birth-year))
                  (str " (" (author :birth-year) " - "))
        death (if (not= nil (or (author :death-year) birth))
                  (str (author :death-year) ")"))]
          (str name birth death)))
; (str (author :name) " (" (author :birth-year) " - " (author :death-year)")"))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors))))

(defn books->string [books]
  (let [books-count (cond
                (>= 0 (count books)) "No books"
                (= 1 (count books)) "1 book. "
                :else (str (count books) " books. "))]
                (str books-count (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (str (% :name))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
