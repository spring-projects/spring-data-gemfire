/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.repository.support;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author David Turanski
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class PersonRepositoryIntegrationTests {
	@Autowired
	PersonRepository repository;
	
	@Test
	public void testCountProjections() {
		Person dave1 = new Person(1L,"Dave","Matthews");
		Person dave2 = new Person(2L,"Dave","Turanski");
		repository.save(dave1);
		repository.save(dave2);
		assertEquals(2L,repository.countByFirstname("Dave"));
		assertEquals(1,repository.countByLastname("Matthews"));
		assertEquals(2,repository.countFirstNameManual("Dave"));
	}
}
