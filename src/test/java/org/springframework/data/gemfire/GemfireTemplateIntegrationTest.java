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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.annotation.Resource;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.query.SelectResults;

/**
 * The GemfireTemplateIntegrationTest class is a test suite of test cases testing the contract and functionality
 * of the SDG GemfireTemplate class for wrapping a GemFire Cache Region and performing Cache Region operations.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.GemfireTemplate
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.4.0
 */
@ContextConfiguration
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class GemfireTemplateIntegrationTest {

	protected static final List<User> TEST_USER_LIST = new ArrayList<User>(11);

	static {
		TEST_USER_LIST.add(createUser("jonDoe"));
		TEST_USER_LIST.add(createUser("janeDoe", false));
		TEST_USER_LIST.add(createUser("pieDoe", false));
		TEST_USER_LIST.add(createUser("cookieDoe"));
		TEST_USER_LIST.add(createUser("jackHandy"));
		TEST_USER_LIST.add(createUser("mandyHandy", false));
		TEST_USER_LIST.add(createUser("randyHandy", false));
		TEST_USER_LIST.add(createUser("sandyHandy"));
		TEST_USER_LIST.add(createUser("imaPigg"));
	}

	@Resource(name = "Users")
	private Region<String, User> users;

	@Autowired
	private GemfireTemplate usersTemplate;

	protected static User createUser(final String username) {
		return createUser(username, true);
	}

	protected static User createUser(final String username, final Boolean active) {
		return createUser(username, String.format("%1$s@companyx.com", username), Calendar.getInstance(), active);
	}

	protected static User createUser(final String username, final String email, final Calendar since, final Boolean active) {
		User user = new User(username);
		user.setActive(Boolean.TRUE.equals(active));
		user.setEmail(email);
		user.setSince(since);
		return user;
	}

	protected String getKey(final User user) {
		return (user != null ? user.getUsername() : null);
	}

	protected User getUser(final String username) {
		for (User user : TEST_USER_LIST) {
			if (user.getUsername().equals(username)) {
				return user;
			}
		}

		return null;
	}

	protected List<User> getUsers(final String... usernames) {
		List<User> users = new ArrayList<User>(usernames.length);
		List<String> usernameList = Arrays.asList(usernames);

		for (User user : TEST_USER_LIST) {
			if (usernameList.contains(user.getUsername())) {
				users.add(user);
			}
		}

		return users;
	}

	protected Map<String, User> getUsersAsMap(final User... users) {
		Map<String, User> userMap = new HashMap<String, User>(users.length);

		for (User user : users) {
			userMap.put(getKey(user), user);
		}

		return userMap;
	}

	protected void assertNullEquals(final Object value1, final Object value2) {
		Assert.assertTrue(value1 == null ? value2 == null : value1.equals(value2));
	}

	@Before
	public void setup() {
		assertNotNull("The 'Users' Region was not properly configured and initialized!", users);

		if (users.isEmpty()) {
			for (User user : TEST_USER_LIST) {
				users.put(getKey(user), user);
			}

			assertFalse(users.isEmpty());
			assertEquals(TEST_USER_LIST.size(), users.size());
		}
	}

	@Test
	public void testContainsKey() {
		assertTrue(usersTemplate.containsKey(getKey(getUser("jonDoe"))));
		assertFalse(usersTemplate.containsKey("dukeNukem"));
	}

	@Test
	@Ignore
	public void testContainsKeyOnServer() {
		assertTrue(usersTemplate.containsKeyOnServer(getKey(getUser("jackHandy"))));
		assertFalse(usersTemplate.containsKeyOnServer("maxPayne"));
	}

	@Test
	public void testContainsValue() {
		assertTrue(usersTemplate.containsValue(getUser("pieDoe")));
		assertFalse(usersTemplate.containsValue(createUser("pieDough")));
	}

	@Test
	public void testContainsValueForKey() {
		assertTrue(usersTemplate.containsValueForKey(getKey(getUser("cookieDoe"))));
		assertFalse(usersTemplate.containsValueForKey("chocolateChipCookieDoe"));
	}

	@Test
	public void testCreate() {
		User bartSimpson = createUser("bartSimpson");

		usersTemplate.create(getKey(bartSimpson), bartSimpson);

		assertTrue(users.containsKey(getKey(bartSimpson)));
		assertTrue(users.containsValueForKey(getKey(bartSimpson)));
		assertTrue(users.containsValue(bartSimpson));
		assertEquals(bartSimpson, users.get(getKey(bartSimpson)));
	}

	@Test
	public void testGet() {
		assertEquals(users.get(getKey(getUser("imaPigg"))), usersTemplate.get(getKey(getUser("imaPigg"))));
		assertNullEquals(users.get("mrT"), usersTemplate.get("mrT"));
	}

	@Test
	public void testPut() {
		User peterGriffon = createUser("peterGriffon");

		assertNull(usersTemplate.put(getKey(peterGriffon), peterGriffon));
		assertEquals(peterGriffon, users.get(getKey(peterGriffon)));
	}

	@Test
	public void testPutIfAbsent() {
		User stewieGriffon = createUser("stewieGriffon");

		assertFalse(users.containsValue(stewieGriffon));
		assertNull(usersTemplate.putIfAbsent(getKey(stewieGriffon), stewieGriffon));
		assertTrue(users.containsValue(stewieGriffon));
		assertEquals(stewieGriffon, usersTemplate.putIfAbsent(getKey(stewieGriffon), createUser("megGriffon")));
		assertEquals(stewieGriffon, users.get(getKey(stewieGriffon)));
	}

	@Test
	public void testRemove() {
		User mandyHandy = users.get(getKey(getUser("mandyHandy")));

		assertNotNull(mandyHandy);
		assertEquals(mandyHandy, usersTemplate.remove(getKey(mandyHandy)));
		assertFalse(users.containsKey(getKey(mandyHandy)));
		assertFalse(users.containsValue(mandyHandy));
		assertFalse(users.containsKey("loisGriffon"));
		assertNull(usersTemplate.remove("loisGriffon"));
		assertFalse(users.containsKey("loisGriffon"));
	}

	@Test
	public void testReplace() {
		User randyHandy = users.get(getKey(getUser("randyHandy")));
		User lukeFluke = createUser("lukeFluke");
		User chrisGriffon = createUser("chrisGriffon");

		assertNotNull(randyHandy);
		assertEquals(randyHandy, usersTemplate.replace(getKey(randyHandy), lukeFluke));
		assertEquals(lukeFluke, users.get(getKey(randyHandy)));
		assertFalse(users.containsValue(randyHandy));
		assertFalse(users.containsValue(chrisGriffon));
		assertNull(usersTemplate.replace(getKey(chrisGriffon), chrisGriffon));
		assertFalse(users.containsValue(chrisGriffon));
	}

	@Test
	public void testReplaceOldValueWithNewValue() {
		User jackHandy = getUser("jackHandy");
		User imaPigg = getUser("imaPigg");

		assertTrue(users.containsValue(jackHandy));
		assertFalse(usersTemplate.replace(getKey(jackHandy), null, imaPigg));
		assertTrue(users.containsValue(jackHandy));
		assertEquals(jackHandy, users.get(getKey(jackHandy)));
		assertTrue(usersTemplate.replace(getKey(jackHandy), jackHandy, imaPigg));
		assertFalse(users.containsValue(jackHandy));
		assertEquals(imaPigg, users.get(getKey(jackHandy)));
	}

	@Test
	public void testGetAllReturnsNoResults() {
		List<String> keys = Arrays.asList("keyOne", "keyTwo", "keyThree");

		Map<String, User> actualUserMapping = usersTemplate.getAll(keys);
		Map<String, User> expectedUserMapping = users.getAll(keys);

		assertEquals(expectedUserMapping, actualUserMapping);
	}

	@Test
	public void testGetAllReturnsResults() {
		List<String> keys = Arrays.asList(getKey(getUser("jonDoe")), getKey(getUser("pieDoe")));

		Map<String, User> actualUserMapping = usersTemplate.getAll(keys);
		Map<String, User> expectedUserMapping = users.getAll(keys);

		assertEquals(actualUserMapping, expectedUserMapping);
	}

	@Test
	public void testPutAll() {
		User batMan = createUser("batMap");
		User spiderMan = createUser("spiderMan");
		User superMan = createUser("superMan");

		Map<String, User> userMap = getUsersAsMap(batMan, spiderMan, superMan);

		assertFalse(users.keySet().containsAll(userMap.keySet()));
		assertFalse(users.values().containsAll(userMap.values()));

		usersTemplate.putAll(userMap);

		assertTrue(users.keySet().containsAll(userMap.keySet()));
		assertTrue(users.values().containsAll(userMap.values()));
	}

	@Test
	public void testQuery() {
		SelectResults<User> queryResults = usersTemplate.query("username LIKE '%Doe'");

		assertNotNull(queryResults);

		List<User> queriedUsers = queryResults.asList();

		assertNotNull(queriedUsers);
		assertFalse(queriedUsers.isEmpty());
		assertEquals(4, queriedUsers.size());
		assertTrue(queriedUsers.containsAll(getUsers("jonDoe", "janeDoe", "pieDoe", "cookieDoe")));
	}

	@Test
	public void testFind() {
		SelectResults<User> findResults = usersTemplate.find("SELECT u FROM /Users u WHERE u.username LIKE $1 AND u.active = $2", "%Doe", true);

		assertNotNull(findResults);

		List<User> usersFound = findResults.asList();

		assertNotNull(usersFound);
		assertFalse(usersFound.isEmpty());
		assertEquals(2, usersFound.size());
		assertTrue(usersFound.containsAll(getUsers("jonDoe", "cookieDoe")));
	}

	@Test
	public void testFindUniqueReturnsResult() {
		User jonDoe = usersTemplate.findUnique("SELECT u FROM /Users u WHERE u.username = $1", "jonDoe");

		assertNotNull(jonDoe);
		assertEquals(getUser("jonDoe"), jonDoe);
	}

	@Test(expected = InvalidDataAccessApiUsageException.class)
	public void testFindUniqueReturnsNoResult() {
		usersTemplate.findUnique("SELECT u FROM /Users u WHERE u.username = $1", "benDover");
	}

}
