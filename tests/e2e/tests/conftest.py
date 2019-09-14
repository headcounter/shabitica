from uuid import uuid4
from base64 import b32encode

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

import pytest


@pytest.fixture
def firefox_options(firefox_options):
    firefox_options.add_argument('--headless')
    firefox_options.add_argument('--width=1920')
    firefox_options.add_argument('--height=1080')
    firefox_options.set_preference('browser.tabs.remote.autostart', False)
    firefox_options.log.level = 'trace'
    return firefox_options


class Shabitica:
    base_url = 'http://shabitica.example.org'
    main_url = f'{base_url}/'
    register_url = f'{base_url}/register'
    logout_url = f'{base_url}/logout'

    def __init__(self, driver):
        self._driver = driver

    @property
    def wait(self):
        return WebDriverWait(self._driver, 20)

    def wait_for_overlay_gone(self):
        overlays = self._driver.find_elements_by_class_name('introjs-overlay')
        for overlay in overlays:
            self.wait.until(EC.staleness_of(overlay))

    def skip_welcome(self):
        self.wait.until(EC.visibility_of_element_located(
            (By.CLASS_NAME, 'welcome-section')
        ))

        css = '#avatar-modal .btn-primary'
        self._driver.find_element_by_css_selector(css).click()

        self.wait.until(EC.visibility_of_element_located(
            (By.CSS_SELECTOR, '.avatar-section.page-2')
        ))
        self._driver.find_element_by_class_name('next').click()

        self.wait.until(EC.visibility_of_element_located(
            (By.CLASS_NAME, 'interests-section')
        ))
        self._driver.find_element_by_class_name('next').click()

        self.wait.until(EC.visibility_of_element_located(
            (By.CLASS_NAME, 'introjs-donebutton')
        )).click()

        self.wait_for_overlay_gone()

        self.wait.until(EC.visibility_of_element_located(
            (By.CSS_SELECTOR, 'textarea.quick-add')
        ))


@pytest.fixture
def shabitica(selenium):
    return Shabitica(selenium)


class ShabiticaUser:
    userid: int
    name: str
    passwd: str
    email: str

    def __init__(self, driver, shabitica: Shabitica,
                 userid: int, name: str, passwd: str, email: str):
        self.userid = userid
        self.name = name
        self.passwd = passwd
        self.email = email

        self._driver = driver
        self._shabitica = shabitica

    def login(self) -> 'ShabiticaUser':
        return self


def register_login_user(selenium, shabitica: Shabitica,
                        name: str, passwd: str, email: str) -> str:
    selenium.get(shabitica.register_url)
    wait = WebDriverWait(selenium, 20)

    wait.until(EC.visibility_of_element_located((By.ID, 'app')))
    selenium.find_element_by_id('usernameInput').send_keys(name)
    selenium.find_element_by_id('emailInput').send_keys(email)
    selenium.find_element_by_id('passwordInput').send_keys(passwd)
    selenium.find_element_by_id('confirmPasswordInput').send_keys(passwd)
    selenium.find_element_by_xpath('//*[text()="Join Habitica"]').click()
    shabitica.skip_welcome()


@pytest.fixture
def create_tempuser(selenium, shabitica):

    def _create_login_user(name=None, passwd=None, email=None):
        if name is None:
            # XXX: This is lossy, but user names have a max length of 20 chars
            name = b32encode(uuid4().bytes).lower().rstrip(b'=')[:20].decode()
        if passwd is None:
            # Doesn't need to be truly unique
            passwd = uuid4().hex[:20]
        if email is None:
            email = f'{uuid4().hex}@example.com'

        userid = register_login_user(selenium, shabitica, name, passwd, email)
        return ShabiticaUser(selenium, shabitica, userid, name, passwd, email)

    return _create_login_user
