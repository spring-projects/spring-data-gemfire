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

package org.springframework.data.gemfire.mapping;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.data.mapping.context.MappingContext;

/**
 * The RegionsTest class is a test suite of test cases testing the contract and functionality of the Regions class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.mapping.Regions
 * @since 1.3.4
 */
@SuppressWarnings("unchecked")
@RunWith(MockitoJUnitRunner.class)
public class RegionsTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Mock
	private MappingContext mockMappingContext;

	private Region mockUsers;
	private Region mockAdminUsers;
	private Region mockGuestUsers;

	private Regions regions;

	protected Region mockRegion(String fullPath) {
		return mockRegion(fullPath.substring(fullPath.lastIndexOf(Region.SEPARATOR) + 1), fullPath);
	}

	protected Region mockRegion(String name, String fullPath) {
		Region mockRegion = mock(Region.class, name);

		when(mockRegion.getName()).thenReturn(name);
		when(mockRegion.getFullPath()).thenReturn(fullPath);

		return mockRegion;
	}

	@Before
	public void setup() {
		mockUsers = mockRegion("/Users");
		mockAdminUsers = mockRegion("/Users/Admin");
		mockGuestUsers = mockRegion("/Users/Guest");

		regions = new Regions(Arrays.asList(mockUsers, mockAdminUsers, mockGuestUsers), mockMappingContext);

		assertThat(regions).isNotNull();
	}

	@After
	public void tearDown() {
		mockUsers = mockAdminUsers = mockGuestUsers = null;
		regions = null;
	}

	@Test
	public void getRegionByEntityTypeReturnsRegionForEntityRegionName() {
		GemfirePersistentEntity<User> mockPersistentEntity = mock(GemfirePersistentEntity.class);

		when(mockPersistentEntity.getRegionName()).thenReturn("Users");
		when(mockMappingContext.getPersistentEntity(eq(User.class))).thenReturn(Optional.of(mockPersistentEntity));

		assertThat(regions.getRegion(User.class)).isEqualTo(mockUsers);
	}

	@Test
	public void getRegionByEntityTypeReturnsRegionForEntityTypeSimpleName() {
		when(mockMappingContext.getPersistentEntity(any(Class.class))).thenReturn(Optional.empty());

		assertThat(regions.getRegion(Users.class)).isEqualTo(mockUsers);
	}

	@Test
	public void getRegionByEntityTypeReturnsNull() {
		when(mockMappingContext.getPersistentEntity(any(Class.class))).thenReturn(Optional.empty());

		assertThat(regions.getRegion(Object.class)).isNull();
	}

	@Test
	public void getRegionWithNullEntityTypeThrowsIllegalArgumentException() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("Entity type must not be null");

		regions.getRegion((Class) null);
	}

	@Test
	public void getRegionWithNameReturnsRegion() {
		assertThat(regions.getRegion("Users")).isSameAs(mockUsers);
		assertThat(regions.getRegion("Admin")).isSameAs(mockAdminUsers);
		assertThat(regions.getRegion("Guest")).isSameAs(mockGuestUsers);
	}

	@Test
	public void getRegionWithPathReturnsRegion() {
		assertThat(regions.getRegion("/Users")).isSameAs(mockUsers);
		assertThat(regions.getRegion("/Users/Admin")).isSameAs(mockAdminUsers);
		assertThat(regions.getRegion("/Users/Guest")).isSameAs(mockGuestUsers);
	}

	@Test
	public void getRegionWithNonExistingNameReturnsNull() {
		assertThat(regions.getRegion("NonExistingRegionName")).isNull();
	}

	@Test
	public void getRegionWithNonExistingPathReturnsNull() {
		assertThat(regions.getRegion("/Non/Existing/Region/Path")).isNull();
	}

	@Test
	public void getRegionWithNullNameNullPathThrowsIllegalArgumentException() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("Region name/path is required");

		regions.getRegion((String) null);
	}

	@Test
	public void iterateRegions() {
		List<Region> actualRegions = new ArrayList<>(3);

		for (Region region : regions) {
			actualRegions.add(region);
		}

		List<Region> expectedRegions = Arrays.asList(mockUsers, mockAdminUsers, mockGuestUsers);

		assertThat(actualRegions).hasSize(expectedRegions.size() * 2);
		assertThat(actualRegions).containsAll(expectedRegions);
	}

	interface Users {
	}
}
