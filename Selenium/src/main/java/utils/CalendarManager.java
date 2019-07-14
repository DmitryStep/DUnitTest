package utils;

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;

//  ласс дл€ работы с элементом интерфейса " алендарь"
public class CalendarManager {

    private static WebElement _calendar;
    private static HashMap<String, Integer> months;

     public CalendarManager(WebElement Calendar) {
        _calendar = Calendar;
        months =  new HashMap<>();
        months.put("яЌ¬ј–№", 1);
        months.put("‘≈¬–јЋ№", 2);
        months.put("ћј–“", 3);
        months.put("јѕ–≈Ћ№", 4);
        months.put("ћј…", 5);
        months.put("»ёЌ№", 6);
        months.put("»ёЋ№", 7);
        months.put("ј¬√”—“", 8);
        months.put("—≈Ќ“яЅ–№", 9);
        months.put("ќ “яЅ–№", 10);
        months.put("ЌќяЅ–№", 11);
        months.put("ƒ≈ јЅ–№", 12);
     }

    // -------------------------------- Calendar WebElements -----------------------------------------------------

    // ƒата начала периода в календаре
    public WebElement BeginMonth() {
        return _calendar.findElement(By.xpath("table[@class=\"month1\"]"));
    }

    // ƒата конца периода в календаре
    public WebElement EndMonth() {
        return _calendar.findElement(By.xpath("table[@class=\"month2\"]"));
    }

    //  нопка "ѕрименить" в календаре
    public WebElement appendBtn() {
        return _calendar.findElement(By.xpath("preceding-sibling::div/input[@class=\"apply-btn\"]"));
    }

     //  нопка "ѕредыдущий мес€ц" в календаре
    public WebElement prevMonth(WebElement Date) {
        return Date.findElement(By.xpath("thead/tr/th/span[@class=\"prev\"]"));
    }

    //  нопка "—ледующий мес€ц" в календаре
    public WebElement nextMonth(WebElement Date) {
        return Date.findElement(By.xpath("thead/tr/th/span[@class=\"next\"]"));
    }

    // Ќадпись с мес€цем в календаре
    public WebElement currMonth(WebElement Date) {
        return Date.findElement(By.xpath("thead/tr/th[@class=\"month-name\"]/div[1]"));
    }

    // Ќадпись с годом в календаре
    public WebElement currYear(WebElement Date) {
        return Date.findElement(By.xpath("thead/tr/th[@class=\"month-name\"]/div[2]"));
    }

    // --------------------------------- Calendar methods and events ------------------------------------------------

    //  лик по кнопке "ѕредыдущий мес€ц"
    public void ClickPrev(WebElement Date) {
        prevMonth(Date).click();
    }

    //  лик по кнопке "—ледующий мес€ц"
    public void ClickNext(WebElement Date) {
        nextMonth(Date).click();
    }

    // ѕолучаем текущий мес€ц в календаре
    public int GetCurrentMonth(WebElement Date) {
        return months.get(currMonth(Date).getText());
    }

    // ѕолучаем текущий год в календаре
    public int GetCurrentYear(WebElement Date) {
        return Integer.parseInt(currYear(Date).getText());
    }

    // ”станавливаем год и мес€ц
    public void SetMonthAndYear(WebElement Date, int Month, int Year) {
        int currentYear = GetCurrentYear(Date);
        int currentMonth = GetCurrentMonth(Date);
        if (Year == currentYear) {
            if (Month > currentMonth) {
                while (Month > currentMonth) {
                    ClickNext(Date);
                    currentMonth++;
                }
            }
            if (Month < currentMonth) {
                while (Month < currentMonth) {
                    ClickPrev(Date);
                    currentMonth--;
                }
            }
        }
        if (Year > currentYear) {
            while (Year > currentYear) {
                ClickNext(Date);
                currentYear++;
            }
        }
        if (Year < currentYear) {
            while (Year < currentYear) {
                ClickPrev(Date);
                currentYear--;
            }
        }
    }

    // ”станавливаем день
    public void SetDay(WebElement Date, int Day) {
        Date.findElement(By.xpath("tbody/tr/td/div[contains(@class, \"day toMonth\") and (normalize-space(text())=" + Integer.toString(Day) + ")]")).click();
    }

    // ”станавливаем полную дату
    public void SetDate(WebElement WEDate, String Dt) {
        Date _date = null;
        try {
            DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
            _date = dateFormat.parse(Dt);
        } catch (ParseException e) {
                e.printStackTrace();
        }
        int Day = _date.getDate();
        int Month = _date.getMonth() + 1;
        int Year = _date.getYear() + 1900;
        SetMonthAndYear(WEDate, Month, Year);
        SetDay(WEDate, Day);
    }


    //  лик по кнопке "ѕрименить"
    public void clickAppendBtn() {
        appendBtn().click();
    }

    // ”станавливаем дату начала
    public void SetBeginDate(String beginDate) {
        SetDate(BeginMonth(), beginDate);
    }

    // ”станавливаем дату конца
    public void SetEndDate(String endDate) {
        SetDate(EndMonth(), endDate);
    }

    // ”станавливаем весь период
    public void SetPeriod(String beginDate, String endDate) {
        Date _date = null;
        int BeginMonth = -1;
        int EndMonth = -1;
        try {
            DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
            _date = dateFormat.parse(beginDate);
            BeginMonth = _date.getMonth() + 1;
            _date = dateFormat.parse(endDate);
            EndMonth = _date.getMonth() + 1;
        } catch (ParseException e) {
            e.printStackTrace();
        }
        SetBeginDate(beginDate);
        if (BeginMonth != EndMonth) {
            SetEndDate(endDate);
        } else {
            SetBeginDate(endDate);
        }
        clickAppendBtn();
    }
}
