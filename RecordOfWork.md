1. Variables have been converted to the proper datatypes and we have removed the `id` variable

* id:	Unique ID for the customer
* Gender:	Gender of the customer
* Age:	Age of the customer
* Driving_License:	0 for Customer does not have DL, 1 for Customer already has DL
* Region_Code:	Unique code for the region of the customer
* Previously_Insured:	1 : Customer already has Vehicle Insurance, 0 : Customer doesn't have Vehicle Insurance. PROBABLY NEEDS TO BE REMOVED. A customer is probably going to renew his insurance contract.
* Vehicle_Age:	Age of the Vehicle
* Vehicle_Damage:	1 : Customer got his/her vehicle damaged in the past. 0 : Customer didn't get his/her vehicle damaged in the past.
* Annual_Premium:	The amount customer needs to pay as premium in the year
* Policy_Sales_Channel:	Anonymized Code for the channel of outreaching to the customer ie. Different Agents, Over Mail, Over Phone, In Person, etc.
* Vintage:	Number of Days, Customer has been associated with the company
* Response:	1 : Customer is interested, 0 : Customer is not interested

We can use he `skim` function of the `skimr` library to check it out. 

2.  