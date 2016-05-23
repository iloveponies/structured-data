(ns structured-data)

(defn do-a-thing [x]
  (let [xx  (+ x x)]
    (Math/pow xx xx)))
;  :-)

(defn spiff [v] (+ (get v 0) (get v 2)))
;  :-)

(defn cutify [v] (conj v "<3"))
;  :-)

(defn spiff-destructuring [v] (let [[x y z] v] (+ x z)))
;  :-)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))
;  :-)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))
;  :-)

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))
;  :-)

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))
;  :-)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x3 y3] point]
     (and (<= x1 x3 x2) (<= y1 y3 y2)))))
;  :-)

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer]
    (let [[[ix1 iy1] [ix2 iy2]] inner]
      (and (<= ox1 ix1) (<= oy1 iy1) (<= ix2 ox2) (<= iy2 oy2)))))
;  :-)

(defn title-length [book] (count (:title book)))
;  :-)

(defn author-count [book] (count (:authors book)))
;  :-)

(defn multiple-authors? [book] (> (count (:authors book)) 1))
;  :-)

(defn add-author [book new-author]
  (let [newauth (book :authors)]
    (let [newnames (conj newauth new-author)]
      (assoc book :authors newnames))))

; a simpler approach
; (defn add-author [book new-author]
;  (merge-with list book {:authors new-author}))

;  :-)

(defn alive? [author] (not (contains? author :death-year)))
;  :-)

(defn element-lengths [collection]
  (let [getlen (fn [elem] (count elem))]
    (map getlen collection)))
;  :-)

(defn second-elements [collection]
  (let [get2 (fn [elem] (get elem 1))]
    (map get2 collection)))
;  :-)

(defn titles [books]
  (let [title (fn [elem] (:title elem))]
    (map title books)))
;  :-)

(defn monotonic? [a-seq] (or (apply <= a-seq) (apply >= a-seq)))
;  :-)

(defn stars [n] (apply str (repeat n "*")))

;  :-)

(defn toggle [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))
;  :-)

(defn contains-duplicates? [a-seq]
  (let [newset (set a-seq)] (not= (count a-seq) (count newset))))
;  :-)

(defn old-book->new-book [book]
   (assoc book :authors (set (concat (:authors book)))))

;  :-)

(defn has-author? [book author] (contains? (:authors book) author))
;  :-)


(defn authors [books]
  (let [author-names
        (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map author-names books)))))

(defn all-author-names [books] (authors books))

(defn all-author-names2 [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

;  :-)

(defn author->string [author]
  (let [auth-name (:name author)
        auth-birth (:birth-year author)
        auth-death (:death-year author)]
    (if (= (str auth-birth auth-death) "") (str auth-name)
                                    (str auth-name " (" auth-birth " - " auth-death ")"))))
;  :-)

(defn authors->string [authors] (apply str (interpose ", " (map author->string authors))))

;  :-)

(defn book->string [book] (str (:title book) ", written by " (authors->string (:authors book))))


;  :-)

(defn books->string [books] (let [num_books (count books)
                                  str_books
                             (cond
                               (= num_books 0) (str "No books")
                               (= num_books 1) (str "1 book. ")
                               (> num_books 1) (str num_books " books. ")
                               :else (str "Error!!!"))]
                             (str str_books (apply str (interpose ". " (map book->string books))) ".")))

;  :-)

(defn books-by-author [author books] (map :title (filter (fn [x] (contains? (:authors x) author)) books)))

; (map :title
;  :-)

(defn author-by-name [name authors] (if (some #(= name (:name %)) authors) name))
;  :-)

(defn living-authors [authors] (map :name (filter (fn [x] (alive? x)) authors)))
;  :-)

(defn has-a-living-author? [book] (if (some #(alive? %) (:authors book)) true false))
;  :-)

(defn books-by-living-authors [books] (map :title (filter (fn [x] (has-a-living-author? x)) books)))
;  :-)

; %________%
