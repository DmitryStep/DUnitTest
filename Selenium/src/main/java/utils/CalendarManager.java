package utils;

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;

// ����� ��� ������ � ��������� ���������� "���������"
public class CalendarManager {

    private static WebElement _calendar;
    private static HashMap<String, Integer> months;

     public CalendarManager(WebElement Calendar) {
        _calendar = Calendar;
        months =  new HashMap<>();
        months.put("������", 1);
        months.put("�������", 2);
        months.put("����", 3);
        months.put("������", 4);
        months.put("���", 5);
        months.put("����", 6);
        months.put("����", 7);
        months.put("������", 8);
        months.put("��������", 9);
        months.put("�������", 10);
        months.put("������", 11);
        months.put("�������", 12);
     }

    // -------------------------------- Calendar WebElements -----------------------------------------------------

    // ���� ������ ������� � ���������
    public WebElement BeginMonth() {
        return _calendar.findElement(By.xpath("table[@class=\"month1\"]"));
    }

    // ���� ����� ������� � ���������
    public WebElement EndMonth() {
        return _calendar.findElement(By.xpath("table[@class=\"month2\"]"));
    }

    // ������ "���������" � ���������
    public WebElement appendBtn() {
        return _calendar.findElement(By.xpath("preceding-sibling::div/input[@class=\"apply-btn\"]"));
    }

     // ������ "���������� �����" � ���������
    public WebElement prevMonth(WebElement Date) {
        return Date.findElement(By.xpath("thead/tr/th/span[@class=\"prev\"]"));
    }

    // ������ "��������� �����" � ���������
    public WebElement nextMonth(WebElement Date) {
        return Date.findElement(By.xpath("thead/tr/th/span[@class=\"next\"]"));
    }

    // ������� � ������� � ���������
    public WebElement currMonth(WebElement Date) {
        return Date.findElement(By.xpath("thead/tr/th[@class=\"month-name\"]/div[1]"));
    }

    // ������� � ����� � ���������
    public WebElement currYear(WebElement Date) {
        return Date.findElement(By.xpath("thead/tr/th[@class=\"month-name\"]/div[2]"));
    }

    // --------------------------------- Calendar methods and events ------------------------------------------------

    // ���� �� ������ "���������� �����"
    public void ClickPrev(WebElement Date) {
        prevMonth(Date).click();
    }

    // ���� �� ������ "��������� �����"
    public void ClickNext(WebElement Date) {
        nextMonth(Date).click();
    }

    // �������� ������� ����� � ���������
    public int GetCurrentMonth(WebElement Date) {
        return months.get(currMonth(Date).getText());
    }

    // �������� ������� ��� � ���������
    public int GetCurrentYear(WebElement Date) {
        return Integer.parseInt(currYear(Date).getText());
    }

    // ������������� ��� � �����
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

    // ������������� ����
    public void SetDay(WebElement Date, int Day) {
        Date.findElement(By.xpath("tbody/tr/td/div[contains(@class, \"day toMonth\") and (normalize-space(text())=" + Integer.toString(Day) + ")]")).click();
    }

    // ������������� ������ ����
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


    // ���� �� ������ "���������"
    public void clickAppendBtn() {
        appendBtn().click();
    }

    // ������������� ���� ������
    public void SetBeginDate(String beginDate) {
        SetDate(BeginMonth(), beginDate);
    }

    // ������������� ���� �����
    public void SetEndDate(String endDate) {
        SetDate(EndMonth(), endDate);
    }

    // ������������� ���� ������
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
