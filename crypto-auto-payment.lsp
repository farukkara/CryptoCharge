(defun pay-with-crypto (car-id crypto-wallet gas-station-address amount)
  (let ((transaction (make-crypto-transaction car-id crypto-wallet gas-station-address amount)))
    (if (transaction-valid? transaction)
        (progn
          (select-crypto-exchange transaction)
          (process-payment transaction)
          (confirm-payment transaction)
          (bluetooth-notification "Payment Successful"))
        (print "Transaction Invalid"))))

(defun make-crypto-transaction (car-id crypto-wallet gas-station-address amount)
  (let ((transaction (make-transaction car-id crypto-wallet gas-station-address amount)))
    (add-crypto-signature transaction crypto-wallet)
    transaction))

(defun select-crypto-exchange (transaction)
  (let ((exchange (best-exchange-for-transaction transaction)))
    (exchange-api-call exchange transaction)))

(defun process-payment (transaction)
  (cond
    ((crypto-transaction? transaction) (process-crypto-payment transaction))
    ((visa-transaction? transaction) (process-visa-payment transaction))
    ((mastercard-transaction? transaction) (process-mastercard-payment transaction))))

(defun confirm-payment (transaction)
  (let ((confirmation (get-confirmation transaction)))
    (if (confirmation-valid? confirmation)
        (update-transaction-status transaction "success")
        (update-transaction-status transaction "failed"))))

(defun bluetooth-notification (message)
  (send-bluetooth-message message))

;; Additional functionality

;; Check if crypto wallet has sufficient funds
(defun check-wallet-funds (crypto-wallet amount)
  (if (> (balance crypto-wallet) amount)
      true
      (print "Insufficient funds in crypto wallet")))

;; Check if gas station address is valid
(defun check-gas-station-address (gas-station-address)
  (let ((valid (valid-address? gas-station-address)))
    (if valid
        true
        (print "Invalid gas station address"))))

;; Check if car is registered for payments
(defun check-car-registration (car-id)
  (let ((registered (car-registered? car-id)))
    (if registered
        true
        (print "Car is not registered for payments"))))

;; Check for ongoing promotions or discounts
(defun check-promotions (gas-station-address amount)
  (let ((promotions (get-promotions gas-station-address)))
    (if (not (empty? promotions))
        (let ((discount (apply-best-promotion amount promotions)))
          (print (format nil "Applied promotion: ~a" discount)))
        true)))

;; Add additional checks to the pay-with-crypto function
(defun pay-with-crypto (car-id crypto-wallet gas-station-address amount)
  (if (and (check-wallet-funds crypto-wallet amount)
           (check
