(ns structured-data)

(defn do-a-thing [x]
  (let [thing (+ x x)]
    (Math/pow thing thing)))

(defn spiff [v]
  (+ (first v) (nth v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[f _ t] v]
    (+ f t)))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (.contains (range x1 (inc x2)) x) (.contains (range y1 (inc y2)) y))))

(defn contains-rectangle? [outer inner]
  (->> (map #(contains-point? outer %) inner)
       (every? true?)))

(defn title-length [book]
  (-> book
      (:title)
      (count)))

(defn author-count [book]
  (-> book
      (:authors)
      (count)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [cur-auths (book :authors)]
    (->> new-author
        (conj cur-auths)
        (assoc book :authors))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(get % 1) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (->> (repeat n \*)
      (apply str)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (set (remove #{elem} a-set))
    (set (conj a-set elem))))

(defn contains-duplicates? [a-seq]
  (->> a-seq
      (frequencies)
      (vals)
      (every? #(= 1 %))
      (not)))

(defn old-book->new-book [book]
  (let [old-auths (book :authors)]
    (assoc book :authors (set old-auths))))

(defn has-author? [book author]
  (not (nil? (some #{author} (book :authors)))))

(defn authors [books]
  (->> books
       (map :authors)
       (apply clojure.set/union)))

(defn all-author-names [books]
  (->> books
       (authors)
       (map :name)
       (set)))

(defn author->string [author]
  (let [form (if (nil? (author :birth-year))
               "%s"
               "%s (%s - %s)")]
    (->> author
         ((juxt :name :birth-year :death-year))
         (map #(if (nil? %1) "" %1))
         (apply format form))))

(defn authors->string [authors]
  (->> authors
       (map author->string)
       (clojure.string/join ", ")))

(defn book->string [book]
  (let [title (book :title)
        old-auths (->> book
                      (:authors)
                      (map author->string)
                      (vec))
        authors (as-> old-auths _
                      (first _)
                      (str "written by " _)
                      (assoc old-auths 0 _))]
    (clojure.string/join ", "
                         (concat [title]
                                 authors))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [bks (map book->string books)
          nbks (count bks)
          nbks-frm (if (= 1 nbks) "%s book" "%s books")
          nbks-str (format nbks-frm nbks)]
      (str
        (clojure.string/join ". " (concat [nbks-str] bks))
        "."))))

(defn books-by-author [author books]
  (->> books
       (filter #(has-author? % author))))

(defn author-by-name [name authors]
  (first (filter #(if (= name (% :name)) %) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (-> book
      (:authors)
      (living-authors)
      (empty?)
      (not)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

