from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC


def test_imageproxy_markdown(create_tempuser, selenium, shabitica):
    create_tempuser().login()
    selenium.implicitly_wait(20)

    qadd = selenium.find_element_by_css_selector('textarea.quick-add')
    qadd.send_keys('Test ![image](http://example.org/image.png)')
    qadd.send_keys(Keys.RETURN)

    img = selenium.find_element_by_class_name('markdown-img')
    proxy_hash = 'aHR0cDovL2V4YW1wbGUub3JnL2ltYWdlLnBuZw'
    expect = f'{shabitica.base_url}/imageproxy/{proxy_hash}'
    assert img.get_attribute('src') == expect


def test_invite_urls(create_tempuser, selenium, shabitica):
    create_tempuser().login()

    shabitica.wait.until(EC.element_to_be_clickable(
        (By.XPATH, '//*[contains(@class, "nav-item")]/a[text()="Party"]')
    )).click()

    shabitica.wait_for_overlay_gone()

    shabitica.wait.until(EC.element_to_be_clickable(
        (By.XPATH, '//button[text()="Create a Party"]')
    )).click()

    shabitica.wait.until(EC.visibility_of_element_located(
        (By.CLASS_NAME, 'introjs-donebutton')
    )).click()

    shabitica.wait_for_overlay_gone()

    shabitica.wait.until(EC.element_to_be_clickable(
        (By.XPATH, '//button[text()="Invite"]')
    )).click()

    shabitica.wait.until(EC.visibility_of_element_located(
        (By.ID, 'invite-modal')
    ))

    shabitica.wait.until(EC.element_to_be_clickable(
        (By.XPATH, '//button[text()="Generate URL"]')
    )).click()

    invite_url = shabitica.wait.until(EC.visibility_of_element_located(
        (By.ID, 'invite-url')
    )).text

    selenium.get(shabitica.logout_url)
    shabitica.wait.until(EC.visibility_of_element_located((By.ID, 'app')))

    selenium.get(invite_url)
    shabitica.wait.until(EC.visibility_of_element_located((By.ID, 'app')))

    selenium.find_element_by_id('usernameInput').send_keys('inviteduser')
    email = selenium.find_element_by_css_selector('input[placeholder="Email"]')
    email.send_keys('invited@example.org')

    for placeholder in ['Password', 'Confirm Password']:
        css = f'input[placeholder="{placeholder}"]'
        selenium.find_element_by_css_selector(css).send_keys('bar')

    selenium.find_element_by_xpath('//*[text()="Sign Up"]').click()
    shabitica.skip_welcome()
