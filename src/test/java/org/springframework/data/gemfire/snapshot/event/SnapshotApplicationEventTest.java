/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.snapshot.event;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotMetadata;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.snapshot.SnapshotOptions;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.data.gemfire.test.support.FileSystemUtils;

/**
 * The SnapshotApplicationEventTest class is a test suite of test cases testing the contract and functionality
 * of the SnapshotApplicationEvent class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.snapshot.event.SnapshotApplicationEvent
 * @since 1.7.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("unchecked")
public class SnapshotApplicationEventTest {

	@Mock
	@SuppressWarnings("unused")
	private Region<Object, Object> mockRegion;

	protected <K, V> SnapshotMetadata<K, V> newSnapshotMetadata() {
		return new SnapshotMetadata<>(FileSystemUtils.WORKING_DIRECTORY, SnapshotOptions.SnapshotFormat.GEMFIRE);
	}

	@Test
	public void constructSnapshotApplicationEventWithoutRegionOrSnapshotMetadata() {

		SnapshotApplicationEvent event = new TestSnapshotApplicationEvent(this);

		assertThat(event.getSource(), is(equalTo(this)));
		assertThat(event.getRegionPath(), is(nullValue()));
		assertThat(event.getSnapshotMetadata(), is(notNullValue()));
		assertThat(event.getSnapshotMetadata().length, is(equalTo(0)));
		assertThat(event.isCacheSnapshotEvent(), is(true));
		assertThat(event.isRegionSnapshotEvent(), is(false));
	}

	@Test
	public void constructSnapshotApplicationEventWithRegionAndSnapshotMetadata() {

		SnapshotMetadata eventSnapshotMetadata = newSnapshotMetadata();

		SnapshotApplicationEvent event = new TestSnapshotApplicationEvent(this, "/Example", eventSnapshotMetadata);

		assertThat(event.getSource(), is(equalTo(this)));
		assertThat(event.getRegionPath(), is(equalTo("/Example")));
		assertThat(event.getSnapshotMetadata()[0], is(sameInstance(eventSnapshotMetadata)));
		assertThat(event.isCacheSnapshotEvent(), is(false));
		assertThat(event.isRegionSnapshotEvent(), is(true));
	}

	@Test
	public void constructSnapshotApplicationEventWithRegionButNoSnapshotMetadata() {

		SnapshotApplicationEvent event = new TestSnapshotApplicationEvent(this, "/Example");

		assertThat(event.getSource(), is(equalTo(this)));
		assertThat(event.getRegionPath(), is(equalTo("/Example")));
		assertThat(event.getSnapshotMetadata(), is(notNullValue()));
		assertThat(event.getSnapshotMetadata().length, is(equalTo(0)));
		assertThat(event.isCacheSnapshotEvent(), is(false));
		assertThat(event.isRegionSnapshotEvent(), is(true));
	}

	@Test
	public void constructSnapshotApplicationEventWithSnapshotMetadataButNoRegion() {

		SnapshotMetadata eventSnapshotMetadataOne = newSnapshotMetadata();
		SnapshotMetadata eventSnapshotMetadataTwo = newSnapshotMetadata();

		SnapshotApplicationEvent event =
			new TestSnapshotApplicationEvent(this, eventSnapshotMetadataOne, eventSnapshotMetadataTwo);

		assertThat(event.getSource(), is(equalTo(this)));
		assertThat(event.getRegionPath(), is(nullValue()));
		assertThat(event.getSnapshotMetadata()[0], is(equalTo(eventSnapshotMetadataOne)));
		assertThat(event.getSnapshotMetadata()[1], is(equalTo(eventSnapshotMetadataTwo)));
		assertThat(event.isCacheSnapshotEvent(), is(true));
		assertThat(event.isRegionSnapshotEvent(), is(false));
	}

	@Test
	public void matchesNullRegionIsFalse() {
		assertThat(new TestSnapshotApplicationEvent(this).matches((Region) null), is(false));
	}

	@Test
	public void matchesNonMatchingRegionIsFalse() {

		Region mockRegion = mock(Region.class, "MockRegion");

		when(mockRegion.getFullPath()).thenReturn("/Example");

		assertThat(new TestSnapshotApplicationEvent(this, "/Prototype").matches(mockRegion), is(false));

		verify(mockRegion, times(1)).getFullPath();
	}

	@Test
	public void matchesMatchingRegionIsTrue() {

		Region mockRegion = mock(Region.class, "MockRegion");

		when(mockRegion.getFullPath()).thenReturn("/Example");

		assertThat(new TestSnapshotApplicationEvent(this, "/Example").matches(mockRegion), is(true));

		verify(mockRegion, times(1)).getFullPath();
	}

	@Test
	public void matchesNonMatchingRegionPathsIsFalse() {

		SnapshotApplicationEvent event = new TestSnapshotApplicationEvent(this, "/Example");

		assertThat(event.getRegionPath(), is(equalTo("/Example")));
		assertThat(event.matches("Example"), is(false));
		assertThat(event.matches("/Sample"), is(false));
		assertThat(event.matches("/Prototype"), is(false));
		assertThat(event.matches("/example"), is(false));
		assertThat(event.matches("/Exam"), is(false));
		assertThat(event.matches("/Xmpl"), is(false));
		assertThat(event.matches("/Ex."), is(false));
		assertThat(event.matches("/E.g."), is(false));
		assertThat(event.matches("/"), is(false));
		assertThat(event.matches("  "), is(false));
		assertThat(event.matches(""), is(false));
		assertThat(event.matches((String) null), is(false));
	}

	@Test
	public void matchesMatchingRegionPathsIsTrue() {

		assertThat(new TestSnapshotApplicationEvent(this, "/Example").matches("/Example"), is(true));
		assertThat(new TestSnapshotApplicationEvent(this, "/").matches("/"), is(true));
		assertThat(new TestSnapshotApplicationEvent(this, "   ").matches(" "), is(true));
		assertThat(new TestSnapshotApplicationEvent(this, "").matches(" "), is(true));
		assertThat(new TestSnapshotApplicationEvent(this, "").matches(""), is(true));
		assertThat(new TestSnapshotApplicationEvent(this, (String) null).matches((String) null), is(true));
	}

	protected static final class TestSnapshotApplicationEvent<K, V> extends SnapshotApplicationEvent<K, V> {

		public TestSnapshotApplicationEvent(Object source, SnapshotMetadata<K, V>... snapshotMetadata) {
			super(source, snapshotMetadata);
		}

		public TestSnapshotApplicationEvent(Object source, String regionPath,
				SnapshotMetadata<K, V>... snapshotMetadata) {
			super(source, regionPath, snapshotMetadata);
		}
	}
}
