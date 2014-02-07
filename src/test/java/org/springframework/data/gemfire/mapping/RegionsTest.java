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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.gemfire.repository.sample.GuestUser;
import org.springframework.data.gemfire.repository.sample.RootUser;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.data.mapping.context.MappingContext;

import com.gemstone.gemfire.cache.Region;

/**
 * The RegionsTest class is a test suite of test cases testing the contract and functionality of the Regions class.
 * <p/>
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.mapping.Regions
 * @since 1.3.4
 */
public class RegionsTest {

	private MappingContext mockMappingContext;

	private Region mockUsers;
	private Region mockAdminUsers;
	private Region mockGuestUsers;

	private Regions regions;

	protected Region createMockRegion(final String fullPath) {
		// NOTE if the Region path does not contain a "/" then lastIndexOf returns -1 and substring is appropriately
		// based on a 0 index then by adding 1, ;-)
		return createMockRegion(fullPath.substring(fullPath.lastIndexOf(Region.SEPARATOR) + 1), fullPath);
	}

	protected Region createMockRegion(final String name, final String fullPath) {
		Region mockRegion = mock(com.gemstone.gemfire.cache.Region.class, name);

		when(mockRegion.getName()).thenReturn(name);
		when(mockRegion.getFullPath()).thenReturn(fullPath);

		return mockRegion;
	}

	@Before
	@SuppressWarnings("unchecked")
	public void setup() {
		mockMappingContext = mock(GemfireMappingContext.class, "GemfireMappingContext");

		mockUsers = createMockRegion("/Users");
		mockAdminUsers = createMockRegion("/Users/Admin");
		mockGuestUsers = createMockRegion("/Users/Guest");

		regions = new Regions(Arrays.<Region<?, ?>>asList(mockUsers, mockAdminUsers, mockGuestUsers),
			mockMappingContext);

		assertNotNull(regions);
	}

	@After
	public void tearDown() {
		mockMappingContext = null;
		mockUsers = mockAdminUsers = mockGuestUsers = null;
		regions = null;
	}

	@Test
	public void testIterateRegions() {
		List<Region> actualRegions = new ArrayList<Region>(3);

		for (Region region : regions) {
			actualRegions.add(region);
		}

		List<Region> expectedRegions = Arrays.asList(mockUsers, mockAdminUsers, mockGuestUsers);

		assertEquals(expectedRegions.size() * 2, actualRegions.size());
		assertTrue(actualRegions.containsAll(expectedRegions));
	}

	@Test
	public void testGetRegionByNameOrPath() {
		assertSame(mockUsers, regions.getRegion("Users"));
		assertSame(mockUsers, regions.getRegion("/Users"));
		assertSame(mockAdminUsers, regions.getRegion("Admin"));
		assertSame(mockAdminUsers, regions.getRegion("/Users/Admin"));
		assertSame(mockGuestUsers, regions.getRegion("Guest"));
		assertSame(mockGuestUsers, regions.getRegion("/Users/Guest"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testGetRegionByNameOrPathWithNullArgument() {
		regions.getRegion((String) null);
	}

	@Test
	public void testGetRegionByDomainClassAndUsingPersistentEntity() {
		GemfirePersistentEntity mockUsersEntity = mock(GemfirePersistentEntity.class, "UsersGemfirePeristentEntity");
		GemfirePersistentEntity mockAdminUserEntity = mock(GemfirePersistentEntity.class, "AdminUserGemfirePeristentEntity");
		GemfirePersistentEntity mockGuestUserEntity = mock(GemfirePersistentEntity.class, "GuestUserGemfirePeristentEntity");

		when(mockMappingContext.getPersistentEntity(User.class)).thenReturn(mockUsersEntity);
		when(mockUsersEntity.getRegionName()).thenReturn("/Users");
		when(mockMappingContext.getPersistentEntity(RootUser.class)).thenReturn(mockAdminUserEntity);
		when(mockAdminUserEntity.getRegionName()).thenReturn("Admin");
		when(mockMappingContext.getPersistentEntity(GuestUser.class)).thenReturn(mockGuestUserEntity);
		when(mockGuestUserEntity.getRegionName()).thenReturn("/Users/Guest");
		when(mockMappingContext.getPersistentEntity(Object.class)).thenReturn(null);

		assertSame(mockUsers, regions.getRegion(User.class));
		assertSame(mockUsers, regions.getRegion(Users.class));
		assertSame(mockAdminUsers, regions.getRegion(RootUser.class));
		assertSame(mockGuestUsers, regions.getRegion(GuestUser.class));
	}

	@Test
	public void testGetRegionByDomainClass() {
		when(mockMappingContext.getPersistentEntity(Object.class)).thenReturn(null);
		assertSame(mockUsers, regions.getRegion(Users.class));
		assertNull(regions.getRegion(User.class));
		assertNull(regions.getRegion(RootUser.class));
		assertNull(regions.getRegion(GuestUser.class));
	}

	@Test
	public void testGetRegionByPersistentEntity() {
		GemfirePersistentEntity mockPersistentEntity = mock(GemfirePersistentEntity.class, "GemfirePersistentEntity");

		when(mockMappingContext.getPersistentEntity(any(Class.class))).thenReturn(mockPersistentEntity);
		when(mockPersistentEntity.getRegionName()).thenReturn("/Non/Existing/Region");

		assertNull(regions.getRegion(User.class));
		assertNull(regions.getRegion(RootUser.class));
		assertNull(regions.getRegion(GuestUser.class));
	}

	protected static interface Users {
	}

}
