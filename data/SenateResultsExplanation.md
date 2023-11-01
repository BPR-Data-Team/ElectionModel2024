#Codebook for U.S. Senate Returns 1976–2018

The data file `1976-2018-senate` contains constituency (state-level) returns for elections to the U.S. Senate from 1976 to 2018.  The data source is the document "[Statistics of the Congressional Election](http://history.house.gov/Institution/Election-Statistics/Election-Statistics/)," published biennially by the Clerk of the U.S. House of Representatives. 2018 data comes from official state election websites (in some cases, they are marked as unofficial, and will be updated at a later time). Uncontested races are identified with a vote total of 1. 

##Variables 
The variables are listed as they appear in the data file.  

###year
 - **Description**: year in which election was held
 
---------------

###state
 - **Description**: state name

 ---------------
 
###state_po
 - **Description**: U.S. postal code state abbreviation

 ---------------
 
###state_fips
 - **Description**: State FIPS code

----------------

###state_cen
 - **Description**: U.S. Census state code

 ---------------
 
### state_ic
 - **Description**: ICPSR state code

 --------------- 
 
###office
- **Description**: U.S. SENATE (constant)
  
---------------

### district
 - **Description**: statewide (constant)

---------------

### stage
 - **Description**: electoral stage
 - **Coding**: 

| code | definition |
|:---|:---|
| "gen" | general elections |
| "runoff" | runoff elections |
| "pri" | primary elections |

 - **Note**: Only appears in special cases. Consult original House Clerk report for these cases.

----------------

### special
- **Description**: special election
- Coding  

| code | definition |
|:---|:---|
| "TRUE" | special elections |
| "FALSE" | regular elections |

----------------

### candidate
  - **Description**: name of the candidate, upper case
  - **Note**: The name is as it appears in the House Clerk report.

----------------

### party_detailed
- **Description**: party of the candidate (always entirely uppercase)
  - **Note**: Parties are as they appear in the House Clerk report. In states that allow candidates to appear on multiple party lines, separate vote totals are indicated for each party.  Therefore, for analysis that involves candidate totals, it will be necessary to aggregate across all party lines within a district.  For analysis that focuses on two-party vote totals, it will be necessary to account for major party candidates who receive votes under multiple party labels. Minnesota party labels are given as they appear on the Minnesota ballots. Future versions of this file will include codes for candidates who are endorsed by major parties, regardless of the party label under which they receive votes.
	
### party_simplified
- **Description**: party of the candidate (always entirely uppercase)
The entries will be one of: DEMOCRAT, REPUBLICAN, LIBERTARIAN, OTHER
	
----------------
	
### writein
- **Description**: vote totals associated with write-in candidates
- **Coding**:

| code | definition |
|:---|:---|
| "TRUE" | write-in candidates |
| "FALSE" | non-write-in candidates |

-----------------

### mode
- **Description**: mode of voting; states with data that doesn't break down returns by mode are marked as "total"

----------------
	
### candidatevotes 
 - **Description**: votes received by this candidate for this particular party

----------------

### totalvotes
 - **Description**: total number of votes cast for this election

 ----------------

### unofficial
- **Description**: TRUE/FALSE indicator for unofficial result (to be updated later); this appears only for 2018 data in some cases

----------------

### version  
- **Description**: date when this dataset was finalize
