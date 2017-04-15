(ns structured-data)

(defn abs [x]
  (if(< x 0)
    (- x)
    x))

(defn do-a-thing [x]
  (let[xx (+ x x)]
    (Math/pow xx xx)))


(defn spiff [v]
   (let[elem1 (get v 0)
        elem2 (get v 2)]
     (+ elem1 elem2)))

(defn cutify [v]
   (conj v "<3"))

(defn spiff-destructuring [v]
  (let[[x y z] v]
  (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
     (abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
     (abs (- y1 y2))))

(defn square? [rectangle]
   (== (height rectangle) (width rectangle) ))

(defn area [rectangle]
   (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle
         [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
   (let[[p1 p2] inner]
     (and (contains-point? outer p1) (contains-point? outer p2)))
  )

(defn title-length [book]
   (count (:title book)))

(defn author-count [book]
   (count (:authors book))
  )

(defn multiple-authors? [book]
   (> (author-count book) 1)
  )

(defn add-author [book new-author]
   (let[author-list (:authors book)
        new-author-list (conj author-list new-author)]
     (assoc book :authors new-author-list)
     ))

(defn alive? [author]
   (not (contains? author :death-year)
        ))

(defn element-lengths [collection]
   (map count collection))

(defn second-elements [collection]
   (let[second-elem (fn [x] (get x 1))]
     (map second-elem collection)))

(defn titles [books]
   (map :title books))

(defn monotonic? [a-seq]
   (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
   (let[starSeq (repeat n "*")]
    (apply str starSeq)))

(defn toggle [a-set elem]
   (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
   (let[setCount (count (set a-seq))]
     (> (count a-seq) setCount)))

(defn old-book->new-book [book]
   (let[authorVector (:authors book)
        authorSet (set authorVector)]
     (assoc book :authors authorSet)))

(defn has-author? [book author]
   (let[newBook (old-book->new-book book)
        authors (:authors newBook)]
     (contains? authors author)))

(defn authors [books]
   (let[newBooks (set (map old-book->new-book books))
        authors (map :authors newBooks)]
     (apply clojure.set/union authors)))

(defn all-author-names [books]
  (let[authorInfo (authors books)]
    (set(map :name authorInfo))
    ))

(defn author->string [author]
   (let[nameStr (:name author)
        birthStr (:birth-year author)
        deathStr (:death-year author)]
     (if(contains? author :birth-year)
     (str nameStr " (" birthStr " - " deathStr ")")
     nameStr)
     ))

(defn authors->string [authors]
   (
    let[converted (map  author->string authors)
        commaAdder (fn [string] (str string ", "))
        withComma (map commaAdder converted)
        concatenated (apply str withComma)
        strLen (count concatenated)]
     (if (> strLen 0)
       (subs concatenated 0 (- strLen 2))
       "")
     ))

(defn book->string [book]
   (let[titleStr (:title book)
        authorsStr (authors->string (:authors book))]
    (str titleStr ", written by " authorsStr)
     ))

(defn books->string [books]
   (let[converted (map book->string books)
        periodAdder (fn [string] (str string ". "))
        withPeriod(map periodAdder converted)
        concatenated (apply str withPeriod)
        strLen (count concatenated)
        bookCount (count converted)]
     (cond
      (> bookCount 1)(str bookCount " books. " (subs concatenated 0 (- strLen 1)))
      (> bookCount 0)(str bookCount " book. " (subs concatenated 0 (- strLen 1)))
      :else "No books.")

    ))

(defn books-by-author [author books]
   (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
   (let[found (filter (fn [auth] (= name (str (:name auth)))) authors)]
     (if(empty? found)
       nil
       (first found)
       )))

(defn living-authors [authors]
   (filter alive? authors))

(defn has-a-living-author? [book]
     (let[aliveAuthors (living-authors (:authors book))]
     (not (empty? aliveAuthors))
     ))

(defn books-by-living-authors [books]
   (filter has-a-living-author? books))

; %________%


