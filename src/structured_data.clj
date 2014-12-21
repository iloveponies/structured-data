(ns structured-data)

(defn do-a-thing [x]
  (let [tx (+ x x)]
  	(Math/pow tx tx)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
  	(if (== (- x2 x1) (- y2 y1))
      true
      false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [xp yp] point]
    (if (and (<= x1 xp x2) (<= y1 yp y2))
        true
        false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2)))
        true
        false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1)
    true
    false))

(defn add-author [book new-author]
    (let [uusi (conj (:authors book) new-author)]
    (assoc book :authors uusi)))

(defn alive? [author]
  (if (contains? author :death-year)
      false
      true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [scnd (fn [cl] (get cl 1))]
    (map scnd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (apply <= a-seq)
  		true
  		(if (apply >= a-seq)
  		    true
  		    false)))

(defn stars [n]
    (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [p1 (count a-seq)
        p2 (count (set a-seq))]
    (if (> p1 p2)
        true
        false)))

(defn old-book->new-book [book]
    (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
  	true
  	false))

(defn authors [books]
    (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [age (fn [a] 
               (cond
               		(contains? a :death-year) (str " (" (:birth-year a) " - " (:death-year a) ")")
               		(contains? a :birth-year) (str " (" (:birth-year a) " - )")
               		:else ""))
        name (fn [a] (:name a))]
    (str (name author) (age author))))
               		

(defn authors->string [authors]
  (let [athrs (fn [a] (author->string a))
        itr (fn [au] (map athrs au))]
    (apply str (interpose ", " (itr authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bks (fn [b] (map book->string b))
       num (fn [b] (cond
       					(== (count b) 1) "1 book. "
       					(> (count b) 0) (str (count books) " books. ")
       					:else "No books"))]
    (str (num books) (apply str (interpose ". " (bks books))) ".")))

(defn books-by-author [author books]
  (let [look (fn [b] (contains? (:authors b) author))]
    (filter look books)))

(defn author-by-name [name authors]
  (let [has (fn [a] (filter (fn [th] (= (:name th) name)) authors))]
    (first (has authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
