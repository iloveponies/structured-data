(ns structured-data)

(defn do-a-thing [x]  ;; Tuplataan syöte
	(let [tupla (+ x x)]
		(Math/pow tupla tupla)) )

(defn spiff [v]  ;; Haetaan 1. ja 3. elementti vektorista
	(+ (get v 0) (get v 2)) )

(defn cutify [v]  ;; Lisätään lempeä vektoriin
	(conj v "<3") )

(defn spiff-destructuring [v] ;; Poistetaan viimeinen elementti vektorista
 	(let [[x y z] v]
		(+ x z)) )

(defn point [x y] 
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] ;; Lasketaan suorakaiteen leveys
 	 (let [[[x1 y1] [x2 y2]] rectangle]
		(- x2 x1)) )

(defn height [rectangle] ;; Lasketaan suorakaiteen korkeus
	(let [[[x1 y1] [x2 y2]] rectangle]
                (- y2 y1)) )

(defn square? [rectangle] ;; Tutkitaan onko suorakaide neliö
	 (let [[[x1 y1] [x2 y2]] rectangle]
		(if (== (- y2 y1) (- x2 x1)) true false)) )

(defn area [rectangle] ;; Lasketaan suorakaiteen pinta-ala
   (let [[[x1 y1] [x2 y2]] rectangle]     
                 (* (- y2 y1) (- x2 x1))) )

(defn contains-point? [rectangle point] ;; Sisältyykö annettu piste suorakaiteeseen
	(let [[[x1 y1] [x2 y2]] rectangle
		[px py] point]
		(if (and (<= x1 px x2) (<= y1 py y2)) true false)) )

(defn contains-rectangle? [outer inner] ;; Sisältyykö inner outeriin
	(let [[[x1 y1] [x2 y2]] inner]
		(if (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2])) true false)) )

(defn title-length [book] ;; Kirjan nimen pituus merkkeinä
	(count (:title book)) )

(defn author-count [book] ;; Tekijöiden lukumäärä
  	(count (:authors book)) )

(defn multiple-authors? [book] ;; Onko tekijöitä useampia
  	(if (< 1 (count (:authors book))) true false) )

(defn add-author [book new-author] ;; Uudet tekijän lisääminen
	(let [authors (:authors book)
		new (conj authors new-author)]
		(assoc book :authors new)) )

(defn alive? [author] ;; Onko tekijä elossa
	(not (contains? author :death-year)) )

(defn element-lengths [collection] ;; Kokoelman koko
  	(map count collection) )

(defn second-elements [collection] ;; Poimitaan kokoelman kokoelmista toiseksi indeksoituvat elementit ja kootaan uusi kokoelma niistä
	(let [toinen (fn [x] (get x 1))]
		(map toinen collection)) )

(defn titles [books]  ;; Kootan kirjojen otsikot
	(map :title books) )

(defn monotonic? [a-seq] ;; Onko sarja monotoninen
	(or (apply >= a-seq) (apply <= a-seq)) )

(defn stars [n] ;; Tehdään tähtiä
	(apply str (repeat n "*")) )

(defn toggle [a-set elem] ;; päälle/pois
	(if (contains? a-set elem)
		(disj a-set elem)
		(conj a-set elem)) )

(defn contains-duplicates? [a-seq] ;; Onko sarjassa duplikaatteja
	(let [set (set a-seq)]
		(not= (count set) (count a-seq))) )

(defn old-book->new-book [book] ;; Vaihdetaan kirjan muotoilua
	 (let [set (set (:authors book))]
		(assoc book :authors set)) )

(defn has-author? [book author] ;; Onko kirjalla annettu tekijä
	(contains? (:authors book) author) )

(defn authors [books] ;; Listataan kaikki tekijät
	(apply clojure.set/union (map :authors books)) )

(defn all-author-names [books] ;; Listataan tekijöiden nimet - tää ei vielä ihan uponnut
	(let [author-names
		(fn [book] (map :name (:authors book)))]
	(set (apply concat (map author-names books)))) )

(defn author->string [author] ;; Tekijä lukukelpoiseen muotoon
	(let [name (:name author)
		birth (:birth-year author)
		death (:death-year author)]
		(cond
			(not birth) (str name)
			:else  (str name " (" birth " - " death ")"))) )

(defn authors->string [authors] ;; Useampi tekijä lukukelpoiseen muotoon
	(apply str (interpose ", " (map author->string authors))) )

(defn book->string [book] ;; Kirja lukukelpoiseen muotoon
	(str (:title book) ", written by " (authors->string (:authors book))) )

(defn books->string [books] ;; Useampi kirja lukukelpoiseen muotoon
	(let [kirjalista (apply str (interpose ". " (map book->string books)))]
		(cond
			(<= (count books) 0) (str "No books.")
			(== (count books)  1) (str "1 book. " kirjalista ".")
			:else (str (count books) " books. " kirjalista "."))) )

(defn books-by-author [author books] ;; Kirjat annetulta tekijältä
	(filter (fn [x] (has-author? x author )) books) )

(defn author-by-name [name authors] ;; Etsii nimeä tekijöistä
	(let [found? (fn [x] (= (:name x) name))]
	  (first(filter found? authors))))

(defn living-authors [authors] ;; Etsii elossa olevat tekijät joukosta
	(filter alive? authors) )

(defn has-a-living-author? [book] ;; Tutkii onko joku kirjan tekijöistä elossa
	(< 0 (count (living-authors (:authors book)))) )

(defn books-by-living-authors [books] ;; Kirjat joilla on elossa olevia tekijöitä
	(filter (fn [book] (has-a-living-author? book)) books) )

; %________%
