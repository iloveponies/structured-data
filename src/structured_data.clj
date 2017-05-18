(ns structured-data)

(defn do-a-thing [x]
  (let [xp (+ x x)]
    (Math/pow xp xp)
    )
  )

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (if (or (= x nil) (= y nil))
      "?"
      (+ x y)
      )
    )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x, y, z] v]
    (if (or (= x nil) (= y nil))
      "?"
      (+ x z)
      )
    )
  )

(defn point [x y]
  [x y]
  )

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]
  )

(defn width [[[x1 y1] [x2 y2]]]
  (if (< x2 x1) (- x1 x2) (- x2 x1))
  )

(defn height [[[x1 y1] [x2 y2]]]
  (if (< y2 y1) (- y1 y2) (- y2 y1))
  )

(defn square? [[[x1 y1] [x2 y2]]]
  (let [xdiff (if (< x2 x1) (- x1 x2) (- x2 x1))
        ydiff (if (< y2 y1) (- y1 y2) (- y2 y1))]
    (if (= xdiff ydiff) true false)
    )
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [[[x1 y1] [x2 y2]] [xp yp]]
  (if (and (or (<= x1 xp x2) (<= x2 xp x1)) (or (<= y1 yp y2) (<= y2 yp y1)))
    true
    false
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner
        p1in (contains-point? outer p1)
        p2in (contains-point? outer p2)
         ]
    (if (and p1in p2in) true false)
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false)
  )

(defn add-author [book new-author]
  (let [names (:authors book)
        newNames (conj names new-author)]
    (assoc book :authors newNames)
    )
  )

(defn alive? [author]
  (if (:death-year author) false true)
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [getsec (fn [vec] (get vec 1))]
    (map getsec collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (let [incr (apply <= a-seq)
        decr (apply >= a-seq)]
    (if (or incr decr) true false)
    )
  )

(defn stars [n]
  (let [star (repeat n "*")]
    (apply str star)
    )
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (let [seqCount (count a-seq)
        setCount (count (set a-seq))]
    (if (< setCount seqCount) true false)
    )
  )

(defn old-book->new-book [book]
  (let [authors (:authors book)
        auSet (set authors)]
    (assoc book :authors auSet)
    )
  )

(defn has-author? [book author]
  (let [authors (:authors book)]
    (if (contains? authors author) true false)
    )
  )

(defn authors [books]
  (let [names (fn [book] (:authors book))]
    (apply clojure.set/union (map names books))
    )
  )

(defn all-author-names [books]
  (let [infos (authors books)
        getNames (fn [author] (:name author))]
    (set (map getNames infos))
    )
  )

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
      (not (= birth nil)) (str name " (" birth " - " death ")")
      :else   (str name)
      )
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [aut (authors->string (:authors book))
        titel (:title book)]
    (apply str titel ", written by " aut)
    )
  )

(defn books->string [books]
  (let [num (count books)]
    (cond
      (= num 0)  "No books."
      (= num 1) (str num " book. " (apply str (interpose ". " (map book->string books))) ".")
      :else      (str num " books. " (apply str (interpose ". " (map book->string books))) ".")
      )
    )
  )

(defn books-by-author [author books]
  (let [fun (fn [book] (has-author? book author))
        fbooks (filter fun books)]
    fbooks
    )
  )

(defn author-by-name [name authors]
  (let [f (fn [x] (= (:name x) name))
        fil (filter f authors)]
    (if (empty? fil) nil  (first fil))
    )
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%




