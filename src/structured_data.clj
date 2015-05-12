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
    (Math/abs(- x1 x2))))


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs(- y1 y2))))

 (defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (Math/abs(- x1 x2)) (Math/abs(- y1 y2))) true false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (Math/abs(- x1 x2)) (Math/abs(- y1 y2)))))

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (if (and (<= x1 x3 x3) (<= x1 x4 x2) (<= y1 y3 y2) (<= y1 y4 y2)) true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1) true false))

(defn add-author [book new-author]
  (let [original book
        new  (assoc original :authors (conj (get book :authors) new-author))]
    new))

(defn alive? [author]
  (if (contains? author :death-year)false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (> (count a-seq) (count (set a-seq))) true false))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))

(defn authors [books]
  (let [author
        (fn [book] (map :authors book))]
    (apply clojure.set/union (author books))))

(defn all-author-names [books]
    (set (map :name (authors books))))


(defn author->string [author]
  (let [nimi (get author :name)
        aika (cond (= (str "") (get author :birth-year)) (str "")
                   (= (str "") (get author :death-year)) (str " ("(get author :birth-year)" - )")
                   :else (str " ("(get author :birth-year)" - "(get author :death-year)")"))]
    (str nimi (if (> (count aika) 6) aika (str "")))))


(defn authors->string [authors]
    (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [knimi (str (get book :title)", ")
        nimet (str "written by "(authors->string (get book :authors)))]
    (str knimi nimet)))


(defn books->string [books]
  (let [maara (cond (= 0 (count books)) "No books"
                    (= 1 (count books)) "1 book. "
                    :else (str (count books)" books. "))
        nimet (apply str(interpose ". " (map book->string books)))]
    (str maara nimet".")))



(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (filter alive? (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
