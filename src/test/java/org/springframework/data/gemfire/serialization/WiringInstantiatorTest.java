/*
 * Copyright 2010-2019 the original author or authors.
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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.awt.Point;
import java.awt.Shape;
import java.beans.Beans;
import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.List;

import org.apache.geode.DataSerializable;
import org.apache.geode.Instantiator;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("simple-config.xml")
public class WiringInstantiatorTest {

	@Autowired
	private ApplicationContext ctx;
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

		public void fromData(DataInput in) throws IOException, ClassNotFoundException {
		}

		public void toData(DataOutput out) throws IOException {
		}
	}

	@SuppressWarnings("serial")
	public static class TemplateWiringBean implements DataSerializable {
		Point point;
		Beans beans;

		public void setBeans(Beans bs) {
			this.beans = bs;
		}

		public void fromData(DataInput in) throws IOException, ClassNotFoundException {
		}

		public void toData(DataOutput out) throws IOException {
		}
	}

	@SuppressWarnings("serial")
	public static class TypeA implements DataSerializable {

		public void fromData(DataInput arg0) throws IOException, ClassNotFoundException {
		}

		public void toData(DataOutput arg0) throws IOException {
		}
	}

	@SuppressWarnings("serial")
	public static class TypeB implements DataSerializable {

		public void fromData(DataInput arg0) throws IOException, ClassNotFoundException {
		}

		public void toData(DataOutput arg0) throws IOException {
		}
	}

	@Test
	public void testAutowiredBean() throws Exception {
		Object instance = instantiator.newInstance();
		assertNotNull(instance);
		assertTrue(instance instanceof AnnotatedBean);
		AnnotatedBean bean = (AnnotatedBean) instance;

		assertNotNull(bean.point);
		assertNotNull(bean.shape);

		assertSame(bean.point, ctx.getBean("point"));
		assertSame(bean.shape, ctx.getBean("area"));
	}

	@Test
	public void testTemplateBean() throws Exception {
		WiringInstantiator instantiator2 = new WiringInstantiator(
				new AsmInstantiatorGenerator().getInstantiator(
				TemplateWiringBean.class, 99));
		instantiator2.setBeanFactory(ctx.getAutowireCapableBeanFactory());
		instantiator2.afterPropertiesSet();

		Object instance = instantiator2.newInstance();

		assertTrue(instance instanceof TemplateWiringBean);
		TemplateWiringBean bean = (TemplateWiringBean) instance;

		assertTrue(bean.point == null);
		assertNotNull(bean.beans);

		assertSame(bean.beans, ctx.getBean("beans"));
	}

	public void testInstantiatorFactoryBean() throws Exception {
		@SuppressWarnings("unchecked")
		List<Instantiator> list = (List<Instantiator>) ctx.getBean("instantiator-factory");
		assertNotNull(list);
		assertEquals(2, list.size());
	}
}
