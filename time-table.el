;;; time-table.el --- Display a table of time-zones -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: https://github.com/zkry/time-table
;; Keywords: time-zones


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Time-table is an Emacs package which allows the visual display of
;; multiple time zones and how their days and nights correspond with
;; one another.

;;; Code:

(defun time-table ()
  "Generate a table for multiple time zones."
  (interactive)
  (switch-to-buffer "*time-table*")
  (unless (eq major-mode 'time-table-mode)
    (time-table-mode)))

(defconst time-table-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "a" #'time-table-add-zone)
      (define-key map "b" #'time-table-backward)
      (define-key map "c" #'time-table-center)
      (define-key map "f" #'time-table-forward)
      (define-key map "g" #'time-table-update)
      (define-key map "h" #'describe-mode)
      (define-key map "n" #'next-line)
      (define-key map "p" #'previous-line)
      (define-key map "q" #'quit-window)
      (define-key map "x" #'time-table-delete-column)))
  "Keymap for elfeed-search-mode.")

(defface time-table-good-face
  '((((class color) (background light)) (:background "#afa"))
    (((class color) (background dark))  (:background "#090")))
  "Face used to display work times."
  :group 'time-table)

(defface time-table-bad-face
  '((((class color) (background light)) (:background "#faa"))
    (((class color) (background dark))  (:background "#900")))
  "Face used to display bad times."
  :group 'time-table)

(defface time-table-warn-face
  '((((class color) (background light)) (:background "#ffa"))
    (((class color) (background dark))  (:background "#aa0")))
  "Face used to display warning times."
  :group 'time-table)

(defvar time-table--time-zones
  '((3 "GMT+03"))
  "Time zones currently being displayed in time-zones buffer.")

(defcustom time-table-default-time-zones
  '()
  "Default time zone names to add in the table display upon startup."
  :group 'time-table
  :type '(repeat string))

(defcustom time-table-good-time-interval
  '(8 17)
  "Time interval (inclusive) to be marked as good (green)."
  :group 'time-table
  :type '(list integer integer))

(defcustom time-table-warning-time-interval
  '(6 21)
  "Time interval (inclusive) to be marked as warning (yellow).

Interval should contain `time-table-good-time-interval'.  What is
not marked as good or warning will be bad."
  :group 'time-table
  :type '(list integer integer))

(defconst time-table--time-zone-offset
  0
  "Hour offset when displaying list of time zones.")

(defun time-table-add-zone ()
  "Add a new zone to the time-table buffer."
  (interactive)
  (let ((tz (time-table--select-time-zone)))
    (add-to-list 'time-table--time-zones tz t)
    (time-table--write-times)))

(defun time-table--remove-nth (n list)
  "Remove the Nth element from LIST non-destructively."
  (if (= 0 n)
      (cdr list)
    (cons (car list) (time-table--remove-nth (1- n) (cdr list)))))

(defun time-table-delete-column ()
  "Delete the column where point is located."
  (interactive)
  (let ((col (/ (current-column) 7)))
    (setq time-table--time-zones (time-table--remove-nth col time-table--time-zones)))
  (time-table--write-times))

(defun time-table-center ()
  "Center the time-table to the current column."
  (interactive)
  (let ((start (point)))
    (let* ((col (/ (current-column) 7))
           (time (/ (car (nth col time-table--time-zones)) 60 60)))
      (setq time-table--time-zone-offset (+ (- time) 1)))
    (time-table--write-times)
    (goto-char start)))

(defun time-table-update ()
  "Redraw the time-table buffer."
  (interactive)
  (time-table--write-times))

(defun time-table-forward ()
  "Move forward a time"
  (interactive)
  (let ((start (point)))
    (forward-char 1)
    (while (and (not (looking-at "\\<[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\\>"))
                (not (= (point) (point-max))))
      (forward-char 1))
    (when (= (point) (point-max))
      (goto-char start))))

(defun time-table-backward ()
  "Move forward a time."
  (interactive)
  (forward-char -1)
  (while (and (not (looking-at "\\<[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\\>"))
              (not (= (point) (point-min))))
    (forward-char -1)))

(defun time-table--face-for-time (time)
  "Return the face for the TIME integer."
  (let ((g time-table-good-time-interval)
        (w time-table-warning-time-interval))
    (cond ((<= (car g) time (cadr g)) 'time-table-good-face)
          ((<= (car w) time (cadr w)) 'time-table-warn-face)
          (t 'time-table-bad-face))))

(defun time-table--write-times ()
  "Write the times to the buffer."
  (let ((base-zone (/ (caar time-table--time-zones) 60 60))
        (inhibit-read-only t))
    (erase-buffer)
    (dotimes (i 24)
      (dolist (tz time-table--time-zones)
        (let* ((hour (mod (+ i (/ (car tz) 60 60) (- base-zone) 24 time-table--time-zone-offset) 24))
               (time-str (format "%02d:00" hour)))
          (insert (propertize time-str 'face (time-table--face-for-time hour))
                  (if (equal tz (last time-table--time-zones))
                      ""
                    "  "))))
      (insert "\n")))
  (let ((header-str " "))
    (dolist (tz time-table--time-zones)
      (let ((tz-str (cadr tz)))
        (setq header-str (concat header-str (format "%-7s" tz-str)))))
    (setq header-line-format header-str))
  (goto-char (point-min)))

(defun time-table-mode ()
  "Major mode for viewing table of time zones."
  (interactive)
  (kill-all-local-variables)
  (use-local-map time-table-mode-map)
  (let ((additional-tzs (mapcar (lambda (tz-str)
                                  (current-time-zone 0 tz-str))
                                time-table-default-time-zones)))
    (setq time-table--time-zones (append `(,(current-time-zone)) additional-tzs)
          major-mode 'time-table-mode
          mode-name "time-table"
          buffer-read-only t
          truncate-lines t
          time-table--time-zone-offset 0))
  (buffer-disable-undo)
  (hl-line-mode)
  (time-table--write-times))

;; TODO: Figure out how to programatically generate this.
(defvar time-table--time-zones-list
  '("Africa/Abidjan"
    "Africa/Accra"
    "Africa/Addis_Ababa"
    "Africa/Algiers"
    "Africa/Asmara"
    "Africa/Bamako"
    "Africa/Bangui"
    "Africa/Banjul"
    "Africa/Bissau"
    "Africa/Blantyre"
    "Africa/Brazzaville"
    "Africa/Bujumbura"
    "Africa/Cairo"
    "Africa/Casablanca"
    "Africa/Ceuta"
    "Africa/Conakry"
    "Africa/Dakar"
    "Africa/Dar_es_Salaam"
    "Africa/Djibouti"
    "Africa/Douala"
    "Africa/El_Aaiun"
    "Africa/Freetown"
    "Africa/Gaborone"
    "Africa/Harare"
    "Africa/Johannesburg"
    "Africa/Juba"
    "Africa/Kampala"
    "Africa/Khartoum"
    "Africa/Kigali"
    "Africa/Kinshasa"
    "Africa/Lagos"
    "Africa/Libreville"
    "Africa/Lome"
    "Africa/Luanda"
    "Africa/Lubumbashi"
    "Africa/Lusaka"
    "Africa/Malabo"
    "Africa/Maputo"
    "Africa/Maseru"
    "Africa/Mbabane"
    "Africa/Mogadishu"
    "Africa/Monrovia"
    "Africa/Nairobi"
    "Africa/Ndjamena"
    "Africa/Niamey"
    "Africa/Nouakchott"
    "Africa/Ouagadougou"
    "Africa/Porto-Novo"
    "Africa/Sao_Tome"
    "Africa/Tripoli"
    "Africa/Tunis"
    "Africa/Windhoek"
    "America/Adak"
    "America/Anchorage"
    "America/Anguilla"
    "America/Antigua"
    "America/Araguaina"
    "America/Argentina/Buenos_Aires"
    "America/Argentina/Catamarca"
    "America/Argentina/Cordoba"
    "America/Argentina/Jujuy"
    "America/Argentina/La_Rioja"
    "America/Argentina/Mendoza"
    "America/Argentina/Rio_Gallegos"
    "America/Argentina/Salta"
    "America/Argentina/San_Juan"
    "America/Argentina/San_Luis"
    "America/Argentina/Tucuman"
    "America/Argentina/Ushuaia"
    "America/Aruba"
    "America/Asuncion"
    "America/Atikokan"
    "America/Bahia"
    "America/Bahia_Banderas"
    "America/Barbados"
    "America/Belem"
    "America/Belize"
    "America/Blanc-Sablon"
    "America/Boa_Vista"
    "America/Bogota"
    "America/Boise"
    "America/Cambridge_Bay"
    "America/Campo_Grande"
    "America/Cancun"
    "America/Caracas"
    "America/Cayenne"
    "America/Cayman"
    "America/Chicago"
    "America/Chihuahua"
    "America/Costa_Rica"
    "America/Creston"
    "America/Cuiaba"
    "America/Curacao"
    "America/Danmarkshavn"
    "America/Dawson"
    "America/Dawson_Creek"
    "America/Denver"
    "America/Detroit"
    "America/Dominica"
    "America/Edmonton"
    "America/Eirunepe"
    "America/El_Salvador"
    "America/Fort_Nelson"
    "America/Fortaleza"
    "America/Glace_Bay"
    "America/Godthab"
    "America/Goose_Bay"
    "America/Grand_Turk"
    "America/Grenada"
    "America/Guadeloupe"
    "America/Guatemala"
    "America/Guayaquil"
    "America/Guyana"
    "America/Halifax"
    "America/Havana"
    "America/Hermosillo"
    "America/Indiana/Indianapolis"
    "America/Indiana/Knox"
    "America/Indiana/Marengo"
    "America/Indiana/Petersburg"
    "America/Indiana/Tell_City"
    "America/Indiana/Vevay"
    "America/Indiana/Vincennes"
    "America/Indiana/Winamac"
    "America/Inuvik"
    "America/Iqaluit"
    "America/Jamaica"
    "America/Juneau"
    "America/Kentucky/Louisville"
    "America/Kentucky/Monticello"
    "America/Kralendijk"
    "America/La_Paz"
    "America/Lima"
    "America/Los_Angeles"
    "America/Lower_Princes"
    "America/Maceio"
    "America/Managua"
    "America/Manaus"
    "America/Marigot"
    "America/Martinique"
    "America/Matamoros"
    "America/Mazatlan"
    "America/Menominee"
    "America/Merida"
    "America/Metlakatla"
    "America/Mexico_City"
    "America/Miquelon"
    "America/Moncton"
    "America/Monterrey"
    "America/Montevideo"
    "America/Montreal"
    "America/Montserrat"
    "America/Nassau"
    "America/New_York"
    "America/Nipigon"
    "America/Nome"
    "America/Noronha"
    "America/North_Dakota/Beulah"
    "America/North_Dakota/Center"
    "America/North_Dakota/New_Salem"
    "America/Nuuk"
    "America/Ojinaga"
    "America/Panama"
    "America/Pangnirtung"
    "America/Paramaribo"
    "America/Phoenix"
    "America/Port-au-Prince"
    "America/Port_of_Spain"
    "America/Porto_Velho"
    "America/Puerto_Rico"
    "America/Punta_Arenas"
    "America/Rainy_River"
    "America/Rankin_Inlet"
    "America/Recife"
    "America/Regina"
    "America/Resolute"
    "America/Rio_Branco"
    "America/Santa_Isabel"
    "America/Santarem"
    "America/Santiago"
    "America/Santo_Domingo"
    "America/Sao_Paulo"
    "America/Scoresbysund"
    "America/Shiprock"
    "America/Sitka"
    "America/St_Barthelemy"
    "America/St_Johns"
    "America/St_Kitts"
    "America/St_Lucia"
    "America/St_Thomas"
    "America/St_Vincent"
    "America/Swift_Current"
    "America/Tegucigalpa"
    "America/Thule"
    "America/Thunder_Bay"
    "America/Tijuana"
    "America/Toronto"
    "America/Tortola"
    "America/Vancouver"
    "America/Whitehorse"
    "America/Winnipeg"
    "America/Yakutat"
    "America/Yellowknife"
    "Antarctica/Casey"
    "Antarctica/Davis"
    "Antarctica/DumontDUrville"
    "Antarctica/Macquarie"
    "Antarctica/Mawson"
    "Antarctica/McMurdo"
    "Antarctica/Palmer"
    "Antarctica/Rothera"
    "Antarctica/South_Pole"
    "Antarctica/Syowa"
    "Antarctica/Troll"
    "Antarctica/Vostok"
    "Arctic/Longyearbyen"
    "Asia/Aden"
    "Asia/Almaty"
    "Asia/Amman"
    "Asia/Anadyr"
    "Asia/Aqtau"
    "Asia/Aqtobe"
    "Asia/Ashgabat"
    "Asia/Atyrau"
    "Asia/Baghdad"
    "Asia/Bahrain"
    "Asia/Baku"
    "Asia/Bangkok"
    "Asia/Barnaul"
    "Asia/Beirut"
    "Asia/Bishkek"
    "Asia/Brunei"
    "Asia/Calcutta"
    "Asia/Chita"
    "Asia/Choibalsan"
    "Asia/Chongqing"
    "Asia/Colombo"
    "Asia/Damascus"
    "Asia/Dhaka"
    "Asia/Dili"
    "Asia/Dubai"
    "Asia/Dushanbe"
    "Asia/Famagusta"
    "Asia/Gaza"
    "Asia/Harbin"
    "Asia/Hebron"
    "Asia/Ho_Chi_Minh"
    "Asia/Hong_Kong"
    "Asia/Hovd"
    "Asia/Irkutsk"
    "Asia/Jakarta"
    "Asia/Jayapura"
    "Asia/Jerusalem"
    "Asia/Kabul"
    "Asia/Kamchatka"
    "Asia/Karachi"
    "Asia/Kashgar"
    "Asia/Kathmandu"
    "Asia/Katmandu"
    "Asia/Khandyga"
    "Asia/Krasnoyarsk"
    "Asia/Kuala_Lumpur"
    "Asia/Kuching"
    "Asia/Kuwait"
    "Asia/Macau"
    "Asia/Magadan"
    "Asia/Makassar"
    "Asia/Manila"
    "Asia/Muscat"
    "Asia/Nicosia"
    "Asia/Novokuznetsk"
    "Asia/Novosibirsk"
    "Asia/Omsk"
    "Asia/Oral"
    "Asia/Phnom_Penh"
    "Asia/Pontianak"
    "Asia/Pyongyang"
    "Asia/Qatar"
    "Asia/Qostanay"
    "Asia/Qyzylorda"
    "Asia/Rangoon"
    "Asia/Riyadh"
    "Asia/Sakhalin"
    "Asia/Samarkand"
    "Asia/Seoul"
    "Asia/Shanghai"
    "Asia/Singapore"
    "Asia/Srednekolymsk"
    "Asia/Taipei"
    "Asia/Tashkent"
    "Asia/Tbilisi"
    "Asia/Tehran"
    "Asia/Thimphu"
    "Asia/Tokyo"
    "Asia/Tomsk"
    "Asia/Ulaanbaatar"
    "Asia/Urumqi"
    "Asia/Ust-Nera"
    "Asia/Vientiane"
    "Asia/Vladivostok"
    "Asia/Yakutsk"
    "Asia/Yangon"
    "Asia/Yekaterinburg"
    "Asia/Yerevan"
    "Atlantic/Azores"
    "Atlantic/Bermuda"
    "Atlantic/Canary"
    "Atlantic/Cape_Verde"
    "Atlantic/Faroe"
    "Atlantic/Madeira"
    "Atlantic/Reykjavik"
    "Atlantic/South_Georgia"
    "Atlantic/St_Helena"
    "Atlantic/Stanley"
    "Australia/Adelaide"
    "Australia/Brisbane"
    "Australia/Broken_Hill"
    "Australia/Currie"
    "Australia/Darwin"
    "Australia/Eucla"
    "Australia/Hobart"
    "Australia/Lindeman"
    "Australia/Lord_Howe"
    "Australia/Melbourne"
    "Australia/Perth"
    "Australia/Sydney"
    "Europe/Amsterdam"
    "Europe/Andorra"
    "Europe/Astrakhan"
    "Europe/Athens"
    "Europe/Belgrade"
    "Europe/Berlin"
    "Europe/Bratislava"
    "Europe/Brussels"
    "Europe/Bucharest"
    "Europe/Budapest"
    "Europe/Busingen"
    "Europe/Chisinau"
    "Europe/Copenhagen"
    "Europe/Dublin"
    "Europe/Gibraltar"
    "Europe/Guernsey"
    "Europe/Helsinki"
    "Europe/Isle_of_Man"
    "Europe/Istanbul"
    "Europe/Jersey"
    "Europe/Kaliningrad"
    "Europe/Kiev"
    "Europe/Kirov"
    "Europe/Lisbon"
    "Europe/Ljubljana"
    "Europe/London"
    "Europe/Luxembourg"
    "Europe/Madrid"
    "Europe/Malta"
    "Europe/Mariehamn"
    "Europe/Minsk"
    "Europe/Monaco"
    "Europe/Moscow"
    "Europe/Oslo"
    "Europe/Paris"
    "Europe/Podgorica"
    "Europe/Prague"
    "Europe/Riga"
    "Europe/Rome"
    "Europe/Samara"
    "Europe/San_Marino"
    "Europe/Sarajevo"
    "Europe/Saratov"
    "Europe/Simferopol"
    "Europe/Skopje"
    "Europe/Sofia"
    "Europe/Stockholm"
    "Europe/Tallinn"
    "Europe/Tirane"
    "Europe/Ulyanovsk"
    "Europe/Uzhgorod"
    "Europe/Vaduz"
    "Europe/Vatican"
    "Europe/Vienna"
    "Europe/Vilnius"
    "Europe/Volgograd"
    "Europe/Warsaw"
    "Europe/Zagreb"
    "Europe/Zaporozhye"
    "Europe/Zurich"
    "GMT"
    "Indian/Antananarivo"
    "Indian/Chagos"
    "Indian/Christmas"
    "Indian/Cocos"
    "Indian/Comoro"
    "Indian/Kerguelen"
    "Indian/Mahe"
    "Indian/Maldives"
    "Indian/Mauritius"
    "Indian/Mayotte"
    "Indian/Reunion"
    "Pacific/Apia"
    "Pacific/Auckland"
    "Pacific/Bougainville"
    "Pacific/Chatham"
    "Pacific/Chuuk"
    "Pacific/Easter"
    "Pacific/Efate"
    "Pacific/Enderbury"
    "Pacific/Fakaofo"
    "Pacific/Fiji"
    "Pacific/Funafuti"
    "Pacific/Galapagos"
    "Pacific/Gambier"
    "Pacific/Guadalcanal"
    "Pacific/Guam"
    "Pacific/Honolulu"
    "Pacific/Johnston"
    "Pacific/Kiritimati"
    "Pacific/Kosrae"
    "Pacific/Kwajalein"
    "Pacific/Majuro"
    "Pacific/Marquesas"
    "Pacific/Midway"
    "Pacific/Nauru"
    "Pacific/Niue"
    "Pacific/Norfolk"
    "Pacific/Noumea"
    "Pacific/Pago_Pago"
    "Pacific/Palau"
    "Pacific/Pitcairn"
    "Pacific/Pohnpei"
    "Pacific/Ponape"
    "Pacific/Port_Moresby"
    "Pacific/Rarotonga"
    "Pacific/Saipan"
    "Pacific/Tahiti"
    "Pacific/Tarawa"
    "Pacific/Tongatapu"
    "Pacific/Truk"
    "Pacific/Wake"
    "Pacific/Wallis")
  "A list of timezone names from tz database.")

(defun time-table--select-time-zone ()
  "Prompt the user for a time zone and return it's time offset and name."
  (let* ((tz-name (completing-read "Select time zone:" time-table--time-zones-list))
         (tz (current-time-zone 0 tz-name)))
    (message (format "%s added as %s" tz-name (cadr tz)))
    tz))

(provide 'time-table)
;;; time-table.el ends here
