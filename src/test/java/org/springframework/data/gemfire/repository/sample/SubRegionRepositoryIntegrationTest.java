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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.repository.Wrapper;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests testing the use of GemFire Repositories on GemFire Cache Subregions.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.repository.Wrapper
 * @see org.springframework.data.gemfire.repository.sample.Programmer
 * @see org.springframework.data.gemfire.repository.sample.ProgrammerRepository
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.Region
 * @link https://jira.springsource.org/browse/SGF-251
 * @link https://jira.springsource.org/browse/SGF-252
 * @since 1.4.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration("subregionRepository.xml")
@SuppressWarnings("unused")
public class SubRegionRepositoryIntegrationTest {

	private static final Map<String, RootUser> ADMIN_USER_DATA = new HashMap<>(5, 0.90f);

	private static final Map<String, GuestUser> GUEST_USER_DATA = new HashMap<>(3, 0.90f);

	private static final Map<String, Programmer> PROGRAMMER_USER_DATA = new HashMap<>(23, 0.90f);

	static {
		createAdminUser("supertool");
		createAdminUser("thor");
		createAdminUser("zeus");
		createGuestUser("bubba");
		createGuestUser("joeblow");
		createProgrammer("AdaLovelace", "Ada");
		createProgrammer("AlanKay", "Smalltalk");
		createProgrammer("BjarneStroustrup", "C++");
		createProgrammer("BrendanEich", "JavaScript");
		createProgrammer("DennisRitchie", "C");
		createProgrammer("GuidoVanRossum", "Python");
		createProgrammer("JamesGosling", "Java");
		createProgrammer("JamesStrachan", "Groovy");
		createProgrammer("JohnBackus", "Fortran");
		createProgrammer("JohnKemeny", "BASIC");
		createProgrammer("JohnMcCarthy", "LISP");
		createProgrammer("JoshuaBloch", "Java");
		createProgrammer("LarryWall", "Perl");
		createProgrammer("MartinOdersky", "Scala");
		createProgrammer("NiklausWirth", "Modula-2");
		createProgrammer("NiklausWirth", "Pascal");
		createProgrammer("ThomasKurtz", "BASIC");
		createProgrammer("YukihiroMatsumoto", "Ruby");
	}

	@Autowired
	private ProgrammerRepository programmersRepo;

	@Resource(name = "/Users/Programmers")
	private Region<String, Programmer> programmers;

	@Resource(name = "/Local/Admin/Users")
	private Region<String, RootUser> adminUsers;

	@Resource(name = "/Local/Guest/Users")
	private Region<String, GuestUser> guestUsers;

	@Autowired
	private GuestUserRepository guestUserRepo;

	@Autowired
	private RootUserRepository adminUserRepo;

	protected static RootUser createAdminUser(final String username) {
		RootUser user = new RootUser(username);
		ADMIN_USER_DATA.put(username, user);
		return user;
	}

	protected static GuestUser createGuestUser(final String username) {
		GuestUser user = new GuestUser(username);
		GUEST_USER_DATA.put(username, user);
		return user;
	}

	protected static RootUser getAdminUser(final String username) {
		List<RootUser> users = getAdminUsers(username);
		return (users.isEmpty() ? null : users.get(0));
	}

	protected static List<RootUser> getAdminUsers(final String... usernames) {
		return getUsers(ADMIN_USER_DATA, usernames);
	}

	protected static GuestUser getGuestUser(final String username) {
		List<GuestUser> users = getGuestUsers(username);
		return (users.isEmpty() ? null : users.get(0));
	}

	protected static List<GuestUser> getGuestUsers(final String... usernames) {
		return getUsers(GUEST_USER_DATA, usernames);
	}

	protected static <T extends User> List<T> getUsers(final Map<String, T> userData, final String... usernames) {
		List<T> users = new ArrayList<T>(usernames.length);

		for (String username : usernames) {
			T user = userData.get(username);

			if (user != null) {
				users.add(user);
			}
		}

		Collections.sort(users);

		return users;
	}

	protected static Programmer createProgrammer(final String username, final String programmingLanguage) {
		Programmer programmer = new Programmer(username);
		programmer.setProgrammingLanguage(programmingLanguage);
		PROGRAMMER_USER_DATA.put(username, programmer);
		return programmer;
	}

	protected static List<Programmer> getProgrammers(final String... usernames) {
		List<Programmer> programmers = new ArrayList<Programmer>(usernames.length);

		for (String username : usernames) {
			Programmer programmer = PROGRAMMER_USER_DATA.get(username);

			if (programmer != null) {
				programmers.add(PROGRAMMER_USER_DATA.get(username));
			}
		}

		Collections.sort(programmers);

		return programmers;
	}

	@Before
	public void setup() {
		assertNotNull("The /Users/Programmers Subregion was null!", programmers);

		if (programmers.isEmpty()) {
			programmers.putAll(PROGRAMMER_USER_DATA);
		}

		assertEquals(PROGRAMMER_USER_DATA.size(), programmers.size());
		assertNotNull("The /Local/Admins/Users Subregion was null!", adminUsers);

		if (adminUsers.isEmpty()) {
			adminUsers.putAll(ADMIN_USER_DATA);
		}

		assertEquals(ADMIN_USER_DATA.size(), adminUsers.size());
		assertNotNull("The /Local/Guest/Users Subregion was null!", guestUsers);

		if (guestUsers.isEmpty()) {
			guestUsers.putAll(GUEST_USER_DATA);
		}

		assertEquals(GUEST_USER_DATA.size(), guestUsers.size());
	}

	@Test
	public void testSubregionRepositoryInteractions() {
		assertThat(programmersRepo.findById("JamesGosling").orElse(null)).isEqualTo(PROGRAMMER_USER_DATA.get("JamesGosling"));

		List<Programmer> javaProgrammers = programmersRepo.findDistinctByProgrammingLanguageOrderByUsernameAsc("Java");

		assertNotNull(javaProgrammers);
		assertFalse(javaProgrammers.isEmpty());
		assertEquals(2, javaProgrammers.size());
		assertEquals(javaProgrammers, getProgrammers("JamesGosling", "JoshuaBloch"));

		List<Programmer> groovyProgrammers = programmersRepo.findDistinctByProgrammingLanguageLikeOrderByUsernameAsc("Groovy");

		assertNotNull(groovyProgrammers);
		assertFalse(groovyProgrammers.isEmpty());
		assertEquals(1, groovyProgrammers.size());
		assertEquals(groovyProgrammers, getProgrammers("JamesStrachan"));

		programmersRepo.save(new Wrapper<>(createProgrammer("RodJohnson", "Java"), "RodJohnson"));
		programmersRepo.save(new Wrapper<>(createProgrammer("GuillaumeLaforge", "Groovy"), "GuillaumeLaforge"));
		programmersRepo.save(new Wrapper<>(createProgrammer("GraemeRocher", "Groovy"), "GraemeRocher"));

		javaProgrammers = programmersRepo.findDistinctByProgrammingLanguageOrderByUsernameAsc("Java");

		assertNotNull(javaProgrammers);
		assertFalse(javaProgrammers.isEmpty());
		assertEquals(3, javaProgrammers.size());
		assertEquals(javaProgrammers, getProgrammers("JamesGosling", "JoshuaBloch", "RodJohnson"));

		groovyProgrammers = programmersRepo.findDistinctByProgrammingLanguageLikeOrderByUsernameAsc("Groovy");

		assertNotNull(groovyProgrammers);
		assertFalse(groovyProgrammers.isEmpty());
		assertEquals(3, groovyProgrammers.size());
		assertEquals(groovyProgrammers, getProgrammers("GraemeRocher", "GuillaumeLaforge", "JamesStrachan"));

		List<Programmer> javaLikeProgrammers = programmersRepo.findDistinctByProgrammingLanguageLikeOrderByUsernameAsc("Java%");

		assertNotNull(javaLikeProgrammers);
		assertFalse(javaLikeProgrammers.isEmpty());
		assertEquals(4, javaLikeProgrammers.size());
		assertEquals(javaLikeProgrammers, getProgrammers("BrendanEich", "JamesGosling", "JoshuaBloch", "RodJohnson"));

		List<Programmer> pseudoCodeProgrammers = programmersRepo.findDistinctByProgrammingLanguageLikeOrderByUsernameAsc("PseudoCode");

		assertNotNull(pseudoCodeProgrammers);
		assertTrue(pseudoCodeProgrammers.isEmpty());
	}

	@Test
	public void testIdenticallyNamedSubregionDataAccess() {
		assertThat(adminUserRepo.findById("supertool").orElse(null)).isEqualTo(getAdminUser("supertool"));
		assertThat(guestUserRepo.findById("joeblow").orElse(null)).isEqualTo(getGuestUser("joeblow"));

		List<RootUser> rootUsers = adminUserRepo.findDistinctByUsername("zeus");

		assertNotNull(rootUsers);
		assertFalse(rootUsers.isEmpty());
		assertEquals(1, rootUsers.size());

		assertEquals(getAdminUser("zeus"), rootUsers.get(0));

		List<GuestUser> guestUsers = guestUserRepo.findDistinctByUsername("bubba");

		assertNotNull(guestUsers);
		assertFalse(guestUsers.isEmpty());
		assertEquals(1, guestUsers.size());
		assertEquals(getGuestUser("bubba"), guestUsers.get(0));
	}
}
