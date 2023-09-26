CREATE TABLE DIS_Inputs
(
  inputs_id INT NOT NULL AUTO_INCREMENT,
  user_id INT NOT NULL,
  num_of_stocks INT NOT NULL,
  initial_investment FLOAT NOT NULL,
  desired_sector_a VARCHAR(125) NOT NULL,
  desired_sector_b VARCHAR(125),
  desired_industry_a VARCHAR(255) NOT NULL,
  desired_industry_b VARCHAR(255),
  risk_profile INT NOT NULL,
  PRIMARY KEY (inputs_id, user_id)
);

CREATE TABLE DIS_Screener
(
  ticker VARCHAR(10) NOT NULL,
  company VARCHAR(255),
  sector VARCHAR(125),
  industry VARCHAR(255),
  country VARCHAR(50),
  date_updated DATE,
  PRIMARY KEY (ticker)
)

	

