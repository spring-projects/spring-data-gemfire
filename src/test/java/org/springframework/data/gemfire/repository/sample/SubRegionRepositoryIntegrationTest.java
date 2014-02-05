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

package org.springframework.data.gemfire.repository.sample;

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

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.repository.Wrapper;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Region;

/**
 * The SubRegionRepositoryTest class is a test suite of test cases testing the use of GemFire Repositories on GemFire
 * Cache Subregions.
 * <p/>
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.repository.Wrapper
 * @see org.springframework.data.gemfire.repository.sample.Programmer
 * @see org.springframework.data.gemfire.repository.sample.ProgrammerRepository
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see com.gemstone.gemfire.cache.Region
 * @since 1.0.0
 */
@ContextConfiguration("subregionRepository.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class SubRegionRepositoryIntegrationTest {

	private static final Map<String, Programmer> PROGRAMMER_DATA = new HashMap<String, Programmer>(23, 0.90f);

	static {
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

	protected static Programmer createProgrammer(final String username, final String programmingLanguage) {
		Programmer programmer = new Programmer(username);
		programmer.setProgrammingLanguage(programmingLanguage);
		PROGRAMMER_DATA.put(username, programmer);
		return programmer;
	}

	protected static List<Programmer> getProgrammers(final String... usernames) {
		List<Programmer> programmers = new ArrayList<Programmer>(usernames.length);

		for (String username : usernames) {
			Programmer programmer = PROGRAMMER_DATA.get(username);

			if (programmer != null) {
				programmers.add(PROGRAMMER_DATA.get(username));
			}
		}

		Collections.sort(programmers);

		return programmers;
	}

	@Before
	public void setup() {
		assertNotNull("The /Users/Programmers Subregion was null!", programmers);

		if (programmers.isEmpty()) {
			programmers.putAll(PROGRAMMER_DATA);
		}

		assertEquals(PROGRAMMER_DATA.size(), programmers.size());
	}

	@Test
	public void testSubregionRepositoryInteractions() {
		assertEquals(PROGRAMMER_DATA.get("JamesGosling"), programmersRepo.findOne("JamesGosling"));

		List<Programmer> javaProgrammers = programmersRepo.findDistinctByProgrammingLanguageOrderByUsernameAsc("Java");

		assertNotNull(javaProgrammers);
		assertFalse(javaProgrammers.isEmpty());
		assertEquals(2, javaProgrammers.size());
		assertTrue(javaProgrammers.containsAll(getProgrammers("JamesGosling", "JoshuaBloch")));

		List<Programmer> groovyProgrammers = programmersRepo.findDistinctByProgrammingLanguageLikeOrderByUsernameAsc("Groovy");

		assertNotNull(groovyProgrammers);
		assertFalse(groovyProgrammers.isEmpty());
		assertEquals(1, groovyProgrammers.size());
		assertTrue(groovyProgrammers.containsAll(getProgrammers("JamesStrachan")));

		programmersRepo.save(new Wrapper<Programmer, String>(createProgrammer("RodJohnson", "Java"), "RodJohnson"));
		programmersRepo.save(new Wrapper<Programmer, String>(createProgrammer("GuillaumeLaforge", "Groovy"), "GuillaumeLaforge"));
		programmersRepo.save(new Wrapper<Programmer, String>(createProgrammer("GraemeRocher", "Groovy"), "GraemeRocher"));

		javaProgrammers = programmersRepo.findDistinctByProgrammingLanguageOrderByUsernameAsc("Java");

		assertNotNull(javaProgrammers);
		assertFalse(javaProgrammers.isEmpty());
		assertEquals(3, javaProgrammers.size());
		assertTrue(javaProgrammers.containsAll(getProgrammers("JamesGosling", "JoshuaBloch", "RodJohnson")));

		groovyProgrammers = programmersRepo.findDistinctByProgrammingLanguageLikeOrderByUsernameAsc("Groovy");

		assertNotNull(groovyProgrammers);
		assertFalse(groovyProgrammers.isEmpty());
		assertEquals(3, groovyProgrammers.size());
		assertTrue(groovyProgrammers.containsAll(getProgrammers("GraemeRocher", "GuillaumeLaforge", "JamesStrachan")));

		List<Programmer> javaLikeProgrammers = programmersRepo.findDistinctByProgrammingLanguageLikeOrderByUsernameAsc("Java%");

		assertNotNull(javaLikeProgrammers);
		assertFalse(javaLikeProgrammers.isEmpty());
		assertEquals(4, javaLikeProgrammers.size());
		assertTrue(javaLikeProgrammers.containsAll(getProgrammers("BrendanEich", "JamesGosling", "JoshuaBloch", "RodJohnson")));

		List<Programmer> pseudoCodeProgrammers = programmersRepo.findDistinctByProgrammingLanguageLikeOrderByUsernameAsc("PseudoCode");

		assertNotNull(pseudoCodeProgrammers);
		assertTrue(pseudoCodeProgrammers.isEmpty());
	}

}
