/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.repository.sample;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The UsingQueryAnnotationExtensionsInUserRepositoryIntegrationTest class is a test suite of test cases testing
 * the contract and functionality of the Spring Data Commons Repository abstraction, Spring Data GemFire Repository
 * extention with custom OQL Query annotations.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.repository.GemfireRepository
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.Region
 * @since 1.7.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class UsingQueryAnnotationExtensionsInUserRepositoryIntegrationTest {

	@Resource(name = "Users")
	private Region<String, User> users;

	@Autowired
	private UsingQueryAnnotationExtensionsInUserRepository userRepository;

	protected static User create(String username) {
		return new User(username);
	}

	protected static List<User> users(String... usernames) {
		List<User> users = new ArrayList<User>(usernames.length);

		for (String username : usernames) {
			users.add(create(username));
		}

		return users;
	}

	protected User put(User user) {
		return put(users, user);
	}

	protected User put(Region<String, User> users, User user) {
		users.put(user.getUsername(), user);
		return user;
	}

	@Before
	public void setup() {
		assertThat(users, is(notNullValue()));

		if (users.isEmpty()) {
			assertThat(users.size(), is(equalTo(0)));

			put(create("jonDoe"));
			put(create("joeDoe"));
			put(create("janeDoe"));
			put(create("cookieDoe"));
			put(create("froDoe"));
			put(create("pieDoe"));
			put(create("sourDoe"));
			put(create("toeDoe"));

			assertThat(users.size(), is(equalTo(8)));
		}
	}

	@Test
	public void queryUsingFindBy() {
		List<User> users = userRepository.findBy("j%Doe");

		assertThat(users, is(notNullValue()));
		assertThat(users.isEmpty(), is(false));
		assertThat(users.size(), is(equalTo(1)));
		assertThat(users.get(0), is(equalTo(create("janeDoe"))));
	}

	@Test
	public void queryUsingFindByUsernameOrderedAndLimited() {
		List<User> users = userRepository.findDistinctByUsernameLikeOrderByUsernameAsc("%o%Doe");

		assertThat(users, is(notNullValue()));
		assertThat(users.isEmpty(), is(false));
		assertThat(users.size(), is(equalTo(5)));
		assertThat(users, is(equalTo(users("cookieDoe", "froDoe", "joeDoe", "jonDoe", "sourDoe"))));
	}

	@Test
	public void queryUsingFindByUsernameOrderedAndUnderLimit() {
		List<User> users = userRepository.findDistinctByUsernameLikeOrderByUsernameAsc("%oo%Doe");

		assertThat(users, is(notNullValue()));
		assertThat(users.isEmpty(), is(false));
		assertThat(users.size(), is(equalTo(1)));
		assertThat(users.get(0), is(equalTo(create("cookieDoe"))));
	}

}
