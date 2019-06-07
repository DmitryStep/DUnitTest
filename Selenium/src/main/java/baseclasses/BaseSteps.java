package baseclasses;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.����;
import cucumber.api.java.ru.��;
import org.junit.Assert;
import org.openqa.selenium.By;
import utils.WebDriverManager;

public class BaseSteps extends WebDriverManager {


    @����("����� = (.*)")
    @Given("URL = (.*)")
    @����("������� (.*)")
    @When("Open (.*)")
    public void openUrl(String url) {
        getUrl(url);
    }

    @��("������� URL = (.*)")
    @Then("Current URL = (.*)")
    public void AssertCurrentUrl(String url) {
        Assert.assertEquals(url, getCurrentUrl());
    }




}
