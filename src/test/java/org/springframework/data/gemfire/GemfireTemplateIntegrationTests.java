/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assume.assumeThat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.annotation.Resource;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.SelectResults;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.data.gemfire.util.CacheUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Integration tests for {@link GemfireTemplate}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.GemfireTemplate
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.4.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class GemfireTemplateIntegrationTests {

	protected static final String DEFAULT_GEMFIRE_LOG_LEVEL = "warning";

	protected static final List<User> TEST_USERS = new ArrayList<User>(9);

	static {
		TEST_USERS.add(newUser("jonDoe"));
		TEST_USERS.add(newUser("janeDoe", false));
		TEST_USERS.add(newUser("pieDoe", false));
		TEST_USERS.add(newUser("cookieDoe"));
		TEST_USERS.add(newUser("jackHandy"));
		TEST_USERS.add(newUser("mandyHandy", false));
		TEST_USERS.add(newUser("randyHandy", false));
		TEST_USERS.add(newUser("sandyHandy"));
		TEST_USERS.add(newUser("imaPigg"));
	}

	@Autowired
	private GemFireCache gemfireCache;

	@Autowired
	private GemfireTemplate usersTemplate;

	@Resource(name = "Users")
	private Region<String, User> users;

	protected static User newUser(String username) {
		return newUser(username, true);
	}

	protected static User newUser(String username, Boolean active) {
		return newUser(username, String.format("%1$s@companyx.com", username), Calendar.getInstance(), active);
	}

	protected static User newUser(String username, String email, Calendar since, Boolean active) {
		User user = new User(username);
		user.setActive(Boolean.TRUE.equals(active));
		user.setEmail(email);
		user.setSince(since);
		return user;
	}

	protected String getKey(User user) {
		return (user != null ? user.getUsername() : null);
	}

	protected User getUser(String username) {
		for (User user : TEST_USERS) {
			if (user.getUsername().equals(username)) {
				return user;
			}
		}

		return null;
	}

	protected List<User> getUsers(String... usernames) {
		List<String> usernameList = Arrays.asList(usernames);
		List<User> users = new ArrayList<User>(usernames.length);

		for (User user : TEST_USERS) {
			if (usernameList.contains(user.getUsername())) {
				users.add(user);
			}
		}

		return users;
	}

	protected Map<String, User> getUsersAsMap(String... usernames) {
		return getUsersAsMap(getUsers(usernames));
	}

	protected Map<String, User> getUsersAsMap(User... users) {
		return getUsersAsMap(Arrays.asList(users));
	}

	protected Map<String, User> getUsersAsMap(Iterable<User> users) {
		Map<String, User> userMap = new HashMap<String, User>();

		for (User user : users) {
			userMap.put(getKey(user), user);
		}

		return userMap;
	}

	protected void assertNullEquals(Object value1, Object value2) {
		assertThat(value1 == null ? value2 == null : value1.equals(value2)).isTrue();
	}

	@Before
	public void setup() {
		assertThat(users).isNotNull();

		if (users.isEmpty()) {
			for (User user : TEST_USERS) {
				users.put(getKey(user), user);
			}

			assertThat(users.isEmpty()).isFalse();
			assertThat(users.size()).isEqualTo(TEST_USERS.size());
		}
	}

	@Test
	public void containsKey() {
		assertThat(usersTemplate.containsKey(getKey(getUser("jonDoe")))).isTrue();
		assertThat(usersTemplate.containsKey("dukeNukem")).isFalse();
	}

	@Test
	public void containsKeyOnServer() {
		assumeThat(CacheUtils.isClient(gemfireCache), is(true));
		assertThat(usersTemplate.containsKeyOnServer(getKey(getUser("jackHandy")))).isTrue();
		assertThat(usersTemplate.containsKeyOnServer("maxPayne")).isFalse();
	}

	@Test
	public void containsValue() {
		assertThat(usersTemplate.containsValue(getUser("pieDoe"))).isTrue();
		assertThat(usersTemplate.containsValue(newUser("pieDough"))).isFalse();
	}

	@Test
	public void containsValueForKey() {
		assertThat(usersTemplate.containsValueForKey(getKey(getUser("cookieDoe")))).isTrue();
		assertThat(usersTemplate.containsValueForKey("chocolateChipCookieDoe")).isFalse();
	}

	@Test
	public void create() {
		User bartSimpson = newUser("bartSimpson");

		usersTemplate.create(getKey(bartSimpson), bartSimpson);

		assertThat(users.containsKey(getKey(bartSimpson))).isTrue();
		assertThat(users.containsValueForKey(getKey(bartSimpson))).isTrue();
		assertThat(users.containsValue(bartSimpson)).isTrue();
		assertThat(users.get(getKey(bartSimpson))).isEqualTo(bartSimpson);
	}

	@Test
	public void get() {
		String key = getKey(getUser("imaPigg"));

		assertThat(usersTemplate.<Object, Object>get(key)).isEqualTo(users.get(key));
		assertNullEquals(users.get("mrT"), usersTemplate.get("mrT"));
	}

	@Test
	public void put() {
		User peterGriffon = newUser("peterGriffon");

		assertThat(usersTemplate.put(getKey(peterGriffon), peterGriffon)).isNull();
		assertThat(users.get(getKey(peterGriffon))).isEqualTo(peterGriffon);
	}

	@Test
	public void putIfAbsent() {
		User stewieGriffon = newUser("stewieGriffon");

		assertThat(users.containsValue(stewieGriffon)).isFalse();
		assertThat(usersTemplate.putIfAbsent(getKey(stewieGriffon), stewieGriffon)).isNull();
		assertThat(users.containsValue(stewieGriffon)).isTrue();
		assertThat(usersTemplate.putIfAbsent(getKey(stewieGriffon), newUser("megGriffon"))).isEqualTo(stewieGriffon);
		assertThat(users.get(getKey(stewieGriffon))).isEqualTo(stewieGriffon);
	}

	@Test
	public void remove() {
		User mandyHandy = users.get(getKey(getUser("mandyHandy")));

		assertThat(mandyHandy).isNotNull();
		assertThat(usersTemplate.<Object, Object>remove(getKey(mandyHandy))).isEqualTo(mandyHandy);
		assertThat(users.containsKey(getKey(mandyHandy))).isFalse();
		assertThat(users.containsValue(mandyHandy)).isFalse();
		assertThat(users.containsKey("loisGriffon")).isFalse();
		assertThat(usersTemplate.<Object, Object>remove("loisGriffon")).isNull();
		assertThat(users.containsKey("loisGriffon")).isFalse();
	}

	@Test
	public void replace() {
		User randyHandy = users.get(getKey(getUser("randyHandy")));
		User lukeFluke = newUser("lukeFluke");
		User chrisGriffon = newUser("chrisGriffon");

		assertThat(randyHandy).isNotNull();
		assertThat(usersTemplate.replace(getKey(randyHandy), lukeFluke)).isEqualTo(randyHandy);
		assertThat(users.get(getKey(randyHandy))).isEqualTo(lukeFluke);
		assertThat(users.containsValue(randyHandy)).isFalse();
		assertThat(users.containsValue(chrisGriffon)).isFalse();
		assertThat(usersTemplate.replace(getKey(chrisGriffon), chrisGriffon)).isNull();
		assertThat(users.containsValue(chrisGriffon)).isFalse();
	}

	@Test
	public void replaceOldValueWithNewValue() {
		User jackHandy = getUser("jackHandy");
		User imaPigg = getUser("imaPigg");

		assertThat(users.containsValue(jackHandy)).isTrue();
		assertThat(usersTemplate.replace(getKey(jackHandy), null, imaPigg)).isFalse();
		assertThat(users.containsValue(jackHandy)).isTrue();
		assertThat(users.get(getKey(jackHandy))).isEqualTo(jackHandy);
		assertThat(usersTemplate.replace(getKey(jackHandy), jackHandy, imaPigg)).isTrue();
		assertThat(users.containsValue(jackHandy)).isFalse();
		assertThat(users.get(getKey(jackHandy))).isEqualTo(imaPigg);
	}

	@Test
	public void getAllReturnsNoResults() {
		List<String> keys = Arrays.asList("keyOne", "keyTwo", "keyThree");
		Map<String, User> users = usersTemplate.getAll(keys);

		assertThat(users).isNotNull();
		assertThat(users).isEqualTo(this.users.getAll(keys));
	}

	@Test
	public void getAllReturnsResults() {
		Map<String, User> users = usersTemplate.getAll(Arrays.asList(
			getKey(getUser("jonDoe")), getKey(getUser("pieDoe"))));

		assertThat(users).isNotNull();
		assertThat(users).isEqualTo(getUsersAsMap(getUser("jonDoe"), getUser("pieDoe")));
	}

	@Test
	public void putAll() {
		User batMan = newUser("batMan");
		User spiderMan = newUser("spiderMan");
		User superMan = newUser("superMan");

		Map<String, User> userMap = getUsersAsMap(batMan, spiderMan, superMan);

		assertThat(users.keySet().containsAll(userMap.keySet())).isFalse();
		assertThat(users.values().containsAll(userMap.values())).isFalse();

		usersTemplate.putAll(userMap);

		assertThat(users.keySet().containsAll(userMap.keySet())).isTrue();
		assertThat(users.values().containsAll(userMap.values())).isTrue();
	}

	@Test
	public void query() {
		SelectResults<User> queryResults = usersTemplate.query("username LIKE '%Doe'");

		assertThat(queryResults).isNotNull();

		List<User> usersFound = queryResults.asList();

		assertThat(usersFound).isNotNull();
		assertThat(usersFound.size()).isEqualTo(4);
		assertThat(usersFound.containsAll(getUsers("jonDoe", "janeDoe", "pieDoe", "cookieDoe"))).isTrue();
	}

	@Test
	public void find() {
		SelectResults<User> findResults = usersTemplate.find("SELECT u FROM /Users u WHERE u.username LIKE $1 AND u.active = $2", "%Doe", true);

		assertThat(findResults).isNotNull();

		List<User> usersFound = findResults.asList();

		assertThat(usersFound).isNotNull();
		assertThat(usersFound.size()).isEqualTo(2);
		assertThat(usersFound.containsAll(getUsers("jonDoe", "cookieDoe"))).isTrue();
	}

	@Test
	public void findUniqueReturnsResult() {
		User jonDoe = usersTemplate.findUnique("SELECT u FROM /Users u WHERE u.username = $1", "jonDoe");

		assertThat(jonDoe).isNotNull();
		assertThat(jonDoe).isEqualTo(getUser("jonDoe"));
	}

	@Test(expected = InvalidDataAccessApiUsageException.class)
	public void findUniqueReturnsNoResult() {
		usersTemplate.findUnique("SELECT u FROM /Users u WHERE u.username = $1", "benDover");
	}

	@Test(expected = InvalidDataAccessApiUsageException.class)
	public void findUnqiueReturnsTooManyResults() {
		usersTemplate.findUnique("SELECT u FROM /Users u WHERE u.username LIKE $1", "%Doe");
	}

	@Configuration
	static class GemfireTemplateConfiguration {

		Properties gemfireProperties() {
			Properties gemfireProperties = new Properties();

			gemfireProperties.setProperty("name", applicationName());
			gemfireProperties.setProperty("mcast-port", "0");
			gemfireProperties.setProperty("log-level", logLevel());
			return gemfireProperties;
		}

		String applicationName() {
			return GemfireTemplateIntegrationTests.class.getName();
		}

		String logLevel() {
			return System.getProperty("gemfire.log-level", DEFAULT_GEMFIRE_LOG_LEVEL);
		}

		@Bean
		CacheFactoryBean gemfireCache() {
			CacheFactoryBean gemfireCache = new CacheFactoryBean();

			gemfireCache.setClose(false);
			gemfireCache.setProperties(gemfireProperties());

			return gemfireCache;
		}

		@Bean(name = "Users")
		LocalRegionFactoryBean<Object, Object> usersRegion(GemFireCache gemfireCache) {
			LocalRegionFactoryBean<Object, Object> usersRegion = new LocalRegionFactoryBean<Object, Object>();

			usersRegion.setCache(gemfireCache);
			usersRegion.setClose(false);
			usersRegion.setPersistent(false);

			return usersRegion;
		}

		@Bean
		GemfireTemplate usersRegionTemplate(Region<Object, Object> simple) {
			return new GemfireTemplate(simple);
		}
	}
}
