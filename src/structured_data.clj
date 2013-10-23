(ns structured-data)

(defn do-a-thing [x] (let [xx (+ x x)
        ]
    (Math/pow xx xx)))

(defn spiff [v] (+ (get v 0) (get v 2)))

(defn cutify [v] (conj v "<3"))


(defn spiff-destructuring [v] (let [xx (get v 0) yy (get v 2)]
               (+ xx yy)))


(defn point [x y]
  [x y])


(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))


(defn height [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)))


(defn square? [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
  (= (- x2 x1) (- y2 y1))))


(defn area [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
  (* (- x2 x1) (- y2 y1))))


(defn contains-point? [rectangle point] (let [[[x1 y1] [x2 y2]] rectangle [x y] point] (and (<= x1 x x2) (<= y1 y y2))))

(defn inner [bottom-left top-right]
  [bottom-left top-right])
(defn outer [bottom-left top-right]
  [bottom-left top-right])
(defn contains-rectangle? [outer inner] (let [[[x1 y1] [x2 y2]] outer [[x3 y3] [x4 y4]] inner] (and (and (<= x1 x3 x2) (<= y1 y3 y2)) (and (<= x1 x4 x2) (<= y1 y4 y2)))))


(defn title-length [book] (count (:title book)) )


(defn author-count [book] (count (:authors book)) )


(defn multiple-authors? [book] (> (count (:authors book)) 1))

(defn wow [x y] (let [ori (:authors x)
   new (conj ori y)]
 new (conj ori y)))


(defn add-author [book new-author] (assoc book :authors (wow book new-author)))


(defn alive? [author] (not= (contains? author :death-year) true))

(defn mun [x]
  (count x))

(defn element-lengths [collection] (map mun collection))


(defn second-elements [collection] (let [tes (fn [y] (get y 1))]
  (map tes collection)))


(defn author-names [book]
   (:title book))

(defn titles [books] (map author-names books))


(defn monotonic? [a-seq] (or (apply <= a-seq) (apply >= a-seq)))


(defn sample [x] (repeat x "*"))
(defn stars [n] (apply str (vec (sample n))))


(defn toggle [a-set elem] (contains? a-set elem) (if (= (contains? a-set elem) true)  (disj a-set elem) (conj a-set elem)))


(defn contains-duplicates? [a-seq] (if (= (count a-seq) (count (vec (distinct a-seq)))) false true))


(defn old-book->new-book [book] (assoc book :authors (set (:authors book))))

(defn has-author? [book author] (contains? (set (:authors book)) author))


(defn authors [books] (let [auth-amu (fn [cu] (:authors cu))]
                   (set (apply concat (map auth-amu books)))))


(defn all-author-names [books] (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))


  (defn goka [x] (:name x ))
(defn joka [x] (:birth-year x))
(defn boka [x] (:death-year x))

 (defn rat [cat] (apply str (apply str (map goka [cat])) " " "("(apply str (map joka [cat])) " " "-" " " (apply str (map boka [cat])) ")"))
(defn ratd [catd] (apply str (apply str (map goka [catd]))))
(defn author->string [author] (if (= (apply str (map joka [author])) "") (ratd author) (rat author)))


(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books] (filter (fn [book] (has-author? book author)) books) )


(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
