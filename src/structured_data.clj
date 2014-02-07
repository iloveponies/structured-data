(ns structured-data
  (:import java.lang.Math))


;; http://iloveponies.github.io/120-hour-epic-sax-marathon/structured-data.html

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
   (+ (v 0) (v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ b]]
  (+ a b))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (Math/abs (- xbl xtr))))   ; (- xtr xbl)

(defn height [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (Math/abs (- ybl ytr))))  ; (- ytr ybl)

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
     (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
 (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
   (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
   (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second-element #(get % 1)]
   (map get-second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
   (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seq-count (count a-seq)
        set-count (count (set a-seq))]
       (not= seq-count set-count)))

(defn old-book->new-book [book]
   (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors (map :authors books)]
    (apply clojure.set/union authors)))

(defn all-author-names [books]
  (let [books-authors (authors books)]
    (set (map :name books-authors))))



(defn author->string [author]
  (let [name (:name author)
        years [(:birth-year author) (:death-year author)]]
    (str name (if (not (empty? (filter #(not (nil? %)) years)))
                (str " (" (first years) " - " (second years) ")")
                ""))))

#_(defn author->string [author]
  (let [name    (:name author)
        birth   (:birth-year author)
        death   (:death-year author)
        years   (if (nil? birth)
                  ""
                  (str " (" birth " - " death ")"))]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)
        book-str-msg (cond
                  (empty? books) "No books"
                  (== 1 book-count) "1 book. "
                  :else (str book-count " books. "))
        books-seq (map book->string books)]
    (str book-str-msg (apply str (interpose ". " books-seq)) ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors) ))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
