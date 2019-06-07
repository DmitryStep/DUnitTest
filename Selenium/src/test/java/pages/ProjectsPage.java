package pages;

import baseclasses.BasePage;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.CacheLookup;
import org.openqa.selenium.support.FindBy;

public class ProjectsPage extends BasePage {

    // ---------------------------------- Page Constructor ------------------------------------------------------

    public ProjectsPage(WebDriver driver){
        super(driver);
    }

    // ---------------------------------- ProjectsPage WebElements ---------------------------------------------

    //  нопка "—оздать новый проект"
    public WebElement newProjectButton(){
        return _driver.findElement(By.xpath(".//*[@id=\"ils-body\"]/div/div[1]/button"));
    }

    @FindBy(xpath=".//*[@id=\"project_date1\"]")
    @CacheLookup
    public WebElement beginDate;

    @FindBy(xpath=".//*[@id=\"project_date2\"]")
    @CacheLookup
    public WebElement endDate;

    @FindBy(xpath=".//*[@id=\"ils-body\"]/div/div[2]/div[2]/button")
    @CacheLookup
    public WebElement showFilterResults;

    @FindBy(xpath=".//*[@id=\"ils-body\"]/div/table")
    @CacheLookup
    public WebElement projectsTable;


}
