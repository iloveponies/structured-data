(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[left _] [right _]] rectangle]
    (- right left)))

(defn height [rectangle]
  (let [[[_ bottom] [_ top]] rectangle]
    (- top bottom)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[left bottom] [right top]] rectangle
        [px py] point]
  (and (<= left px right )
       (<= bottom py top))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
 (> (author-count book) 1) )

(defn add-author [book new-author]
  (let [cur-authors (get book :authors)] 
  (assoc book :authors (conj cur-authors new-author))))

(defn alive? [author]
 (if (get author :death-year) false true)) 

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [snd (fn [x] (get x 1))]
  (map snd collection)))

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
 (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors-set (set (:authors book))] 
  (assoc book :authors authors-set)))

(defn has-author? [book author]
 (contains? (:authors book) author))

(defn authors [books]
  (let [authors (fn [book] (:authors book))]
  (set (apply concat (map authors books)))))

(defn all-author-names [books]
  (set (map :name (authors books)))) 

(defn author->string [author]
  (let [auth-name (:name author)]
  (if (:birth-year author)
    (str auth-name " (" (:birth-year author) " - " (:death-year author) ")")
    auth-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        bauthor (:authors book)]
  (str title ", written by " (authors->string bauthor))))

(defn books->string [books]
  (let [bcount (count books)
        blist (apply str (interpose ", " (map book->string books)))]
  (str (cond 
         (= bcount 0) (str "No books.")
         (= bcount 1) (str "1 book. " blist ".")
         :else (str bcount " books. " blist ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [bauthors (:authors book)]
   (not (empty? (filter alive? bauthors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
