(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [prem (get v 0)
        third (get v 2)
        sum 0]
    (+ (if third third 0)
      (if prem prem 0))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+
     (if a a 0)
     (if c c 0))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (Math/abs (- x1 x2)))

(defn height [[[x1 y1] [x2 y2]]]
  (Math/abs (- y1 y2)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point?
  [[[x1 y1] [x2 y2]] ; rect
   [x y]]           ;point
  (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle? [
    outer ; outer rect
    [tl br]] ; inner rect
  (and (contains-point? outer tl)
       (contains-point? outer br)))

(defn title-length [book]
  (let [title (:title book)]
    (count title)))

(defn author-count [book]
  (let [authors (:authors book)]
    (count authors)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new (conj authors new-author)]
        (assoc book :authors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (let [rep (repeat n "*")]
    (apply str rep)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))


(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        yob (:birth-year author)
        yod (:death-year author)
        yobyod (
                if (or yob yod)
                 (str " (" yob " - " yod ")"))]
    (str name yobyod)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [numBooks (count books)
        pre (cond (= 0 numBooks) "No books"
                  (= 1 numBooks) "1 book"
                  :else (str numBooks " books"))
        result (apply str
      (interpose ". "
        (cons pre
              (map book->string books))))]
    (str result ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [found
        (filter (fn [author] (= (:name author) name)) authors)]

    (if (first found)
      (first found)
      nil)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty?
        (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
