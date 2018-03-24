(ns structured-data)

(defn do-a-thing [x]
      (let [x+x (+ x x)]
           (Math/pow x+x x+x)))

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
           (- x2 x1)
           ))

(defn height [rectangle]
      (let [[[x1 y1] [x2 y2]] rectangle]
           (- y2 y1)
           ))

(defn square? [rectangle]
      (if (= (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
      (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
      (let [[[x1 y1] [x2 y2]]  rectangle
            [p1 p2]  point]
            (if (and (<= x1 p1 x2) (<= y1 p2 y2)) true false)))

(defn contains-rectangle? [outer inner]
      (let [[p1 p2]  inner]
           (if (and (contains-point? outer p1) (contains-point? outer p2)) true false)))

(defn title-length [book]
      (count (:title book)))

(defn author-count [book]
      (count (:authors book)))

(defn multiple-authors? [book]
      (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
      (let [ old-auth (:authors book)
             new-auth (conj old-auth new-author)]
           (assoc book :authors new-auth)))

(defn alive? [author]
      (not (contains? author :death-year)))

(defn element-lengths [collection]
      (map count collection))

(defn second-elements [collection]
      (let [second-element (fn [x] (get x 1))]
           (map second-element collection)))

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
      (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
      (let [old-auth (:authors book)
            new-auth (set old-auth)]
           (assoc book :authors new-auth)))

(defn has-author? [book author]
      (contains? (:authors book) author))

(defn authors [books]
      (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
      (set (map :name (authors books))))

(defn author->string [author]
      (let [name (:name author)
            by (:birth-year author)
            dy (:death-year author)]
           (cond
             (and (= by nil) (= dy nil)) (str name)
             (= dy nil) (str name " (" by " - )")
             :else (str name " (" by " - " dy ")")
             )))

(defn authors->string [authors]
      (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
      (let [title (:title book)
            auths (apply str (authors->string (:authors book)))]
           (str title ", written by " auths)))

(defn books->string [books]
      (let [blen (count books)
            bookkss (apply str (interpose ". " (map book->string books)))]
           (cond
             (== blen 0) "No books."
             (== blen 1) (str blen " book. "  bookkss ".")
             (>= blen 2) (str blen " books. " bookkss "."))))

(defn books-by-author [author books]
      (let [has-author?2
            (fn [book] (has-author? book author))]
           (filter has-author?2 books)))

(defn author-by-name [name authors]
      (let [checkName
            (fn [author] (= (:name author) name))]
           (first (filter checkName authors))))

(defn living-authors [authors]
      (filter alive? authors))

(defn has-a-living-author? [book]
      (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
      (filter has-a-living-author? books))

; %________%