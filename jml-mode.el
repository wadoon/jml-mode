(require 'polymode)
(require 'jml-contract-mode)


(defcustom pm-host/java
  (pm-bchunkmode "java" :mode 'java-mode)
  "Java host chunkmode"
  :group 'hostmodes
  :type 'object)

(require 'rx)
(defcustom  pm-inner/jml-contract
  (pm-hbtchunkmode "jml-contract"
		   :mode 'jml-contract-mode
		   :head-reg  (rx "/*@")
		   :tail-reg  (rx "*/"))
  "JML typical chunk."
  :group 'innermodes
  :type 'object)



(defcustom pm-poly/jml
  (pm-polymode-one "jml"
                   :hostmode 'pm-host/java
                   :innermode 'pm-inner/jml-contract)
  "JML typical polymode."
  :group 'polymodes
  :type 'object)

(define-polymode poly-jml-mode pm-poly/jml)

;;
