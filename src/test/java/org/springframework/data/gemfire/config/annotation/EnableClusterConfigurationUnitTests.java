/*
 * Copyright 2017-2019 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.apache.geode.cache.RegionShortcut;
import org.junit.Test;
import org.springframework.core.env.Environment;
import org.springframework.core.type.AnnotationMetadata;

/**
 * Unit tests for {@link EnableClusterConfiguration} annotation and the {@link ClusterConfigurationConfiguration} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.springframework.core.env.Environment
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableClusterConfiguration
 * @since 2.0.1
 */
public class EnableClusterConfigurationUnitTests {

	@Test
	public void setImportMetadataFromAnnotationAttributes() {

		AnnotationMetadata mockImportMetadata = mock(AnnotationMetadata.class);

		Map<String, Object> annotationAttributes = new HashMap<>();

		annotationAttributes.put("host", "skullbox");
		annotationAttributes.put("port", 12345);
		annotationAttributes.put("serverRegionShortcut", RegionShortcut.PARTITION_PERSISTENT);
		annotationAttributes.put("useHttp", true);

		when(mockImportMetadata.getAnnotationAttributes(EnableClusterConfiguration.class.getName()))
			.thenReturn(annotationAttributes);

		when(mockImportMetadata.hasAnnotation(eq(EnableClusterConfiguration.class.getName()))).thenReturn(true);

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration.setImportMetadata(mockImportMetadata);

		assertThat(configuration.getManagementHttpHost().orElse(null)).isEqualTo("skullbox");
		assertThat(configuration.getManagementHttpPort().orElse(0)).isEqualTo(12345);
		assertThat(configuration.getManagementUseHttp().orElse(false)).isTrue();
		assertThat(configuration.getServerRegionShortcut().orElse(null)).isEqualTo(RegionShortcut.PARTITION_PERSISTENT);

		verify(mockImportMetadata, times(1))
			.getAnnotationAttributes(eq(EnableClusterConfiguration.class.getName()));

		verify(mockImportMetadata, times(1))
			.hasAnnotation(eq(EnableClusterConfiguration.class.getName()));
	}

	@Test
	public void setImportMetadataFromProperties() {

		AnnotationMetadata mockImportMetadata = mock(AnnotationMetadata.class);

		Map<String, Object> annotationAttributes = new HashMap<>();

		annotationAttributes.put("host", "skullbox");
		annotationAttributes.put("port", 12345);
		annotationAttributes.put("serverRegionShortcut", RegionShortcut.PARTITION_PERSISTENT);
		annotationAttributes.put("useHttp", false);

		when(mockImportMetadata.getAnnotationAttributes(EnableClusterConfiguration.class.getName()))
			.thenReturn(annotationAttributes);

		when(mockImportMetadata.hasAnnotation(eq(EnableClusterConfiguration.class.getName()))).thenReturn(true);

		Environment mockEnvironment = mock(Environment.class);

		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.cluster.region.type"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.host"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.port"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.use-http"))).thenReturn(true);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.cluster.region.type"), eq(RegionShortcut.class), any(RegionShortcut.class)))
			.thenReturn(RegionShortcut.LOCAL);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.host"), eq(String.class), any(String.class)))
			.thenReturn("cardboardBox");

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.port"), eq(Integer.class), any(Integer.class)))
			.thenReturn(11235);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.use-http"), eq(Boolean.class), any(Boolean.class)))
			.thenReturn(true);

		when(mockEnvironment.resolveRequiredPlaceholders(anyString()))
			.thenAnswer(invocation -> invocation.getArgument(0));

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration.setEnvironment(mockEnvironment);
		configuration.setImportMetadata(mockImportMetadata);

		assertThat(configuration.getManagementHttpHost().orElse(null)).isEqualTo("cardboardBox");
		assertThat(configuration.getManagementHttpPort().orElse(0)).isEqualTo(11235);
		assertThat(configuration.getManagementUseHttp().orElse(false)).isTrue();
		assertThat(configuration.getServerRegionShortcut().orElse(null)).isEqualTo(RegionShortcut.LOCAL);

		verify(mockImportMetadata, times(1))
			.getAnnotationAttributes(eq(EnableClusterConfiguration.class.getName()));

		verify(mockImportMetadata, times(1))
			.hasAnnotation(eq(EnableClusterConfiguration.class.getName()));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.cluster.region.type"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.host"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.port"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.use-http"));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.cluster.region.type"), eq(RegionShortcut.class),
				eq(RegionShortcut.PARTITION_PERSISTENT));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.host"), eq(String.class), anyString());

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.port"), eq(Integer.class), anyInt());

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.use-http"), eq(Boolean.class), eq(false));
	}

	@Test
	public void setImportMetadataFromAnnotationAttributesAndProperties() {

		AnnotationMetadata mockImportMetadata = mock(AnnotationMetadata.class);

		Map<String, Object> annotationAttributes = new HashMap<>();

		annotationAttributes.put("host", "postOfficeBox");
		annotationAttributes.put("port", 10101);
		annotationAttributes.put("serverRegionShortcut", RegionShortcut.REPLICATE);
		annotationAttributes.put("useHttp", true);

		when(mockImportMetadata.getAnnotationAttributes(EnableClusterConfiguration.class.getName()))
			.thenReturn(annotationAttributes);

		when(mockImportMetadata.hasAnnotation(eq(EnableClusterConfiguration.class.getName()))).thenReturn(true);

		Environment mockEnvironment = mock(Environment.class);

		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.cluster.region.type"))).thenReturn(false);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.host"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.port"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.use-http"))).thenReturn(false);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.host"), eq(String.class), any(String.class)))
			.thenReturn("shoeBox");

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.port"), eq(Integer.class), any(Integer.class)))
			.thenReturn(12480);

		when(mockEnvironment.resolveRequiredPlaceholders(anyString()))
			.thenAnswer(invocation -> invocation.getArgument(0));

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration.setEnvironment(mockEnvironment);
		configuration.setImportMetadata(mockImportMetadata);

		assertThat(configuration.getManagementHttpHost().orElse(null)).isEqualTo("shoeBox");
		assertThat(configuration.getManagementHttpPort().orElse(0)).isEqualTo(12480);
		assertThat(configuration.getManagementUseHttp().orElse(false)).isTrue();
		assertThat(configuration.getServerRegionShortcut().orElse(null)).isEqualTo(RegionShortcut.REPLICATE);

		verify(mockImportMetadata, times(1))
			.getAnnotationAttributes(eq(EnableClusterConfiguration.class.getName()));

		verify(mockImportMetadata, times(1))
			.hasAnnotation(eq(EnableClusterConfiguration.class.getName()));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.cluster.region.type"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.host"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.port"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.use-http"));

		verify(mockEnvironment, never())
			.getProperty(eq("spring.data.gemfire.cluster.region.type"), eq(RegionShortcut.class),
				any(RegionShortcut.class));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.host"), eq(String.class), anyString());

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.port"), eq(Integer.class), anyInt());

		verify(mockEnvironment, never())
			.getProperty(eq("spring.data.gemfire.management.use-http"), eq(Boolean.class), anyBoolean());
	}
}
