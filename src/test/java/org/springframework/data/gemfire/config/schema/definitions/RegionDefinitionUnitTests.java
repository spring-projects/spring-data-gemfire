/*
 * Copyright 2017-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.schema.definitions;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.schema.SchemaObjectType;
import org.springframework.data.gemfire.test.support.IOUtils;

/**
 * Unit tests for {@link RegionDefinition}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.schema.definitions.RegionDefinition
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class RegionDefinitionUnitTests {

	@Mock
	private Region<?, ?> mockRegion;

	@Before
	public void setup() {
		assertThat(this.mockRegion).isNotNull();
		when(this.mockRegion.getName()).thenReturn("MockRegion");
	}

	@Test
	public void fromRegionCreatesRegionDefinition() {

		RegionDefinition regionDefinition = RegionDefinition.from(this.mockRegion);

		assertThat(regionDefinition).isNotNull();
		assertThat(regionDefinition.getName()).isEqualTo(this.mockRegion.getName());
		assertThat(regionDefinition.getRegion()).isSameAs(this.mockRegion);
		assertThat(regionDefinition.getRegionShortcut()).isEqualTo(RegionDefinition.DEFAULT_REGION_SHORTCUT);
		assertThat(regionDefinition.getType()).isEqualTo(SchemaObjectType.REGION);
	}

	@Test(expected = IllegalArgumentException.class)
	public void fromNullRegionThrowsIllegalArgumentException() {

		try {
			RegionDefinition.from(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Region is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void create() {

		GemfireAdminOperations mockAdminOperations = mock(GemfireAdminOperations.class);

		RegionDefinition regionDefinition = RegionDefinition.from(this.mockRegion);

		assertThat(regionDefinition).isNotNull();
		assertThat(regionDefinition.getRegion()).isSameAs(this.mockRegion);

		regionDefinition.create(mockAdminOperations);

		verify(mockAdminOperations, times(1)).createRegion(eq(regionDefinition));
	}

	@Test
	public void havingRegionShortcut() {

		RegionDefinition regionDefinition = RegionDefinition.from(this.mockRegion);

		assertThat(regionDefinition).isNotNull();
		assertThat(regionDefinition.getRegion()).isSameAs(this.mockRegion);
		assertThat(regionDefinition.getRegionShortcut()).isEqualTo(RegionDefinition.DEFAULT_REGION_SHORTCUT);
		assertThat(regionDefinition.having(RegionShortcut.LOCAL)).isSameAs(regionDefinition);
		assertThat(regionDefinition.getRegionShortcut()).isEqualTo(RegionShortcut.LOCAL);
		assertThat(regionDefinition.having(null)).isSameAs(regionDefinition);
		assertThat(regionDefinition.getRegionShortcut()).isEqualTo(RegionDefinition.DEFAULT_REGION_SHORTCUT);
		assertThat(regionDefinition.having(RegionShortcut.REPLICATE)).isSameAs(regionDefinition);
		assertThat(regionDefinition.getRegionShortcut()).isEqualTo(RegionShortcut.REPLICATE);
	}

	@Test
	public void withName() {

		RegionDefinition regionDefinition = RegionDefinition.from(this.mockRegion);

		assertThat(regionDefinition).isNotNull();
		assertThat(regionDefinition.getRegion()).isSameAs(this.mockRegion);
		assertThat(regionDefinition.getName()).isEqualTo(this.mockRegion.getName());
		assertThat(regionDefinition.with("/Test")).isSameAs(regionDefinition);
		assertThat(regionDefinition.getName()).isEqualTo("/Test");
		assertThat(regionDefinition.with("  ")).isSameAs(regionDefinition);
		assertThat(regionDefinition.getName()).isEqualTo(this.mockRegion.getName());
		assertThat(regionDefinition.with("/Mock")).isSameAs(regionDefinition);
		assertThat(regionDefinition.getName()).isEqualTo("/Mock");
		assertThat(regionDefinition.with(null)).isSameAs(regionDefinition);
		assertThat(regionDefinition.getName()).isEqualTo(this.mockRegion.getName());
	}

	@Test
	public void serializeDeserializeIsSuccessful() throws IOException, ClassNotFoundException {

		RegionDefinition regionDefinition = RegionDefinition.from(this.mockRegion).having(RegionShortcut.REPLICATE);

		byte[] regionDefinitionBytes = IOUtils.serializeObject(regionDefinition);

		RegionDefinition deserializedRegionDefinition = IOUtils.deserializeObject(regionDefinitionBytes);

		assertThat(deserializedRegionDefinition).isNotNull();
		assertThat(deserializedRegionDefinition.getRegion()).isNull();
		assertThat(deserializedRegionDefinition.getName()).isEqualTo("MockRegion");
		assertThat(deserializedRegionDefinition.getRegionShortcut()).isEqualTo(RegionShortcut.REPLICATE);
	}
}
