(ns structured-data)

(defn hypotenuse [x y]
  (let [xx (* x x)
        yy (* y y)]
    (Math/sqrt (+ xx yy))))


(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx )))

(defn spiff [v]
  (let [ f (get v 0)
        t (get v 2)]
    (+ f t)))

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1)
         (contains-point? outer point2) )))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book))) 

;; conj 
(defn add-author [book new-author]
  (let [authors (:authors book)]
  (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [x] (get x 1))]
    (map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq )
  (apply >= a-seq )))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (let [x (set a-seq)]
    (> (count a-seq) (count x))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))


;; (def china {:name "China Miéville", :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})

;; (def cities {:title "The City and the City" :authors #{china}})
;; (def wild-seed {:title "Wild Seed", :authors #{octavia}})
;; (def embassytown {:title "Embassytown", :authors #{china}})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors #{friedman, felleisen}})

;; (def books [cities, wild-seed, embassytown, little-schemer])

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


;; (defn all-author-names [books]
;;   (let [author-names
;;         (fn [book] (map :name (:authors book)))]
;;     (set (apply concat (map author-names books))) ))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)
        fullstr (str name " (" birth " - " death ")")]
    (if (or birth death) fullstr name)))

(defn authors->string [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))
        

(defn books->string [books]
  (let [number (count books)]
    (cond
      (= 0 number) "No books."
      (= 1 number) (str number " book. " (book->string (first books)) ".")
      :else (str number " books. "
                 (clojure.string/join ". "(map book->string books)) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
