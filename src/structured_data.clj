(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))


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
  (if (== (height rectangle) (width rectangle))
    true
    false))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(area (rectangle [0 0] [4 3]))



(defn contains-point? [rectangle point]
 (let [ [[[x1 y1] [x2 y2]] [px py]] [rectangle point]]
  (if (and (<= x1 px x2) (<= y1 py y2))
  true
  false)))


(defn contains-rectangle? [outer inner]
  (let [ [[a b] [c d]] inner ]
    (cond
      (not (contains-point? outer [a b])) false
      (not (contains-point? outer [c d])) false
      :else true)))

  




(defn title-length [book]
  (count (get book :title)))




(defn author-count [book]
  (count (get book :authors) ))



(defn multiple-authors? [book]
  (if (< 1 (author-count book))
    true
    false))





(defn add-author [book new-author]
  (let [original (get book :authors)
        updated (conj original new-author)]
    (assoc book :authors updated)))
    


(defn alive? [author]
  (not (contains? author :death-year)))







(defn element-lengths [collection]
  (map count collection))




(defn second-elements [collection]
  (let [ota-toka (fn [x] (get x 1))]
    (map ota-toka collection)))
             





(defn titles [books]
  (let [title (fn [book] (:title book))]
    (map title books)))







(defn monotonic? [a-seq]
  (if (or (apply >= a-seq) (apply <= a-seq))
          true
          false))








(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))




(defn contains-duplicates? [a-seq]
  (if (== (count (set a-seq)) (count a-seq))
    false
    true))








(defn old-book->new-book [book]
  (let [old-auth (get book :authors)
       new-auth (set old-auth)]
  (assoc book :authors new-auth)))






(defn has-author? [book author]
  (let [kirja (old-book->new-book book)
        set-of-authors (get kirja :authors)]
    (contains? set-of-authors author)))




(defn authors [books]
(apply clojure.set/union (map :authors (map old-book->new-book books))))

  
(defn all-author-names [books]
  (set (map :name (authors books))))



(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if (not (= death nil))
        (str name " (" birth " - " death ")")
      (if (not (= birth nil))
        (str name " (" birth " - )")
        (str name)))))
        
      



(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors))))





       
(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))



(defn books->string [books]
  (let [amount (count books)
        text-books (str amount " books. ")
        authors (interpose ". " (map book->string books))
        text-authors (apply str authors)]
    (cond  
      (= 0 amount ) (str "No books.")
      (= 1 amount ) (str "1 book. " text-authors ".")
      :else (str text-books text-authors "."))))





(defn books-by-author [author books]
  (let [true-book? (fn [x] (has-author? x author))]
    (filter true-book? books)))



(defn author-by-name [name authors]
  (let [nimi-on-name (fn [x] (= name (:name x)))
        kelpaava (filter nimi-on-name authors)]
    (if (empty? kelpaava)
      nil
      (first kelpaava))))


;(defn author-by-name [name authors]
;  (let 


    


(defn living-authors [authors]
  (let [elossa (fn [x] (= (:death-year x) nil))]
    (filter elossa authors)))





(defn has-a-living-author? [book]
  (let [living (living-authors (:authors book))]
    (not (empty? living))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))



; %________%
