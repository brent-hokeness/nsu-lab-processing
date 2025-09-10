# nsu-lab-processing
Various scripts used for processing trials collected in the NSU biomechanics lab

full_event_mark_v8 
  - Event marks and processes vertical jump data from a folder of txt files exported using the BB VJ analysis file
  - Won't work with other export file formats but could be easily modified as long as the event marking logic can find the force plate      data

VertJumpWithThorax
  - Just like it sounds. Used to process vertical jumps when athletes wore the thorax cluster
  - Doesn't automatically event mark

Soccer Analysis
  - Processes vertical jump trials exported under the soccer workspace/analysis file
  - No event marking
  - Could easily be modified to other sports

NSU_BB_Shiny
  - Dashboard that will display various performance metrics collected on the baseball team
  - Could be very useful for automating reporting and making it super intuitive for the coaching staff to view
  - Would probably need to modify it to best fit your needs but that should be fairly easy
  - Would recommend setting up a task scheduler so the new data appended to the Google sheet will auto populate without having to          manually run the script every time there is new data

txt_to_xls
  - Just like it sounds, converts txt files to excel format
  - Same functionality as the macros, just prefered to do it this way - either way works the same

Good luck with the rest of the files. They are from quite awhile ago. Likely pull max/average/min values across phases or at specific event marks to use for pitching reports and whatnot. 

Feel free to reach out to me with any questions about the functionality of this code or if you need help at brentdhokeness@gmail.com
