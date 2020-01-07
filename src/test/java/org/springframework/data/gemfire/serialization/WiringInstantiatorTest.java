/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire.serialization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.awt.Point;
import java.awt.Shape;
import java.beans.Beans;
import java.io.DataInput;
import java.io.DataOutput;
import java.util.List;

import org.apache.geode.DataSerializable;
import org.apache.geode.Instantiator;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @author Costin Leau
 */
@RunWith(SpringRunner.class)
@ContextConfiguration("simple-config.xml")
@SuppressWarnings("unused")
public class WiringInstantiatorTest {

	@Autowired
	private ApplicationContext applicationContext;

	@Autowired
	private WiringInstantiator instantiator;


	@SuppressWarnings("serial")
	public static class AnnotatedBean implements DataSerializable {

		@Autowired
		Point point;
		Shape shape;

		@Autowired
		void initShape(Shape shape) {
			this.shape = shape;
		}

		public void fromData(DataInput in) { }

		public void toData(DataOutput out) { }

	}

	@SuppressWarnings("serial")
	public static class TemplateWiringBean implements DataSerializable {

		Beans beans;
		Point point;

		public void setBeans(Beans bs) {
			this.beans = bs;
		}

		public void fromData(DataInput in) { }

		public void toData(DataOutput out) { }

	}

	@SuppressWarnings("serial")
	public static class TypeA implements DataSerializable {

		public void fromData(DataInput arg0) { }

		public void toData(DataOutput arg0) { }

	}

	@SuppressWarnings("serial")
	public static class TypeB implements DataSerializable {

		public void fromData(DataInput arg0) { }

		public void toData(DataOutput arg0) { }

	}

	@Test
	public void testAutowiredBean() {

		Object instance = instantiator.newInstance();

		assertNotNull(instance);
		assertTrue(instance instanceof AnnotatedBean);

		AnnotatedBean bean = (AnnotatedBean) instance;

		assertNotNull(bean.point);
		assertNotNull(bean.shape);

		assertSame(bean.point, applicationContext.getBean("point"));
		assertSame(bean.shape, applicationContext.getBean("area"));
	}

	@Test
	public void testTemplateBean() {

		WiringInstantiator instantiator2 =
			new WiringInstantiator(new AsmInstantiatorGenerator().getInstantiator(TemplateWiringBean.class, 99));

		instantiator2.setBeanFactory(applicationContext.getAutowireCapableBeanFactory());
		instantiator2.afterPropertiesSet();

		Object instance = instantiator2.newInstance();

		assertTrue(instance instanceof TemplateWiringBean);
		TemplateWiringBean bean = (TemplateWiringBean) instance;

		assertNull(bean.point);
		assertNotNull(bean.beans);

		assertSame(bean.beans, applicationContext.getBean("beans"));
	}

	public void testInstantiatorFactoryBean() {
		@SuppressWarnings("unchecked")
		List<Instantiator> list = (List<Instantiator>) applicationContext.getBean("instantiator-factory");
		assertNotNull(list);
		assertEquals(2, list.size());
	}
}
